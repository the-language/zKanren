;;  zKanren: MicroKanren with Constraints and noto
;;  Copyright (C) 2017  Zaoqi

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.

;;  You should have received a copy of the GNU Affero General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#lang racket
(define (stream) (error))
(define (stream-cons) (error))
(require "stream.rkt")
(require "monad.rkt")
(provide (all-defined-out))

(define max-size 32)

#| Nat → Var |#
(struct var (v))

#| Num → Num |#
(define (succ x) (+ 1 x))
(define (pred x) (- x 1))

#| Pos = Positive-Integer |#

#| Nat → a → Promise+ a |#
(define-syntax-rule (sized n x) (raw-sized n (delay/name x)))
(define (raw-sized n x) (if (zero? n) (force x) (delay/name (sized (- n 1) x))))

#| Promise+ a = U a (Promise (Promise+ a)) |#

#| Promise+ a → a |#
(define (force+ x) (if (promise? x) (force+ (force x)) x))

#| Promise+ a → (a → b) → Promise+ b |#
(define (fmap x f) (if (promise? x) (delay/name (fmap (force x) f)) (f x)))

#| Promise a → (a → b) → Promise b |#
(define-syntax-rule (fmap-1 x f) (delay/name (f (force x))))

#| a → ((a → b) → b) |#
(define (($ x) f) (f x))

#| [a] → (a → Bool) → Values (Maybe a) [a] |#
(define (find-a xs f)
  (let loop ([as xs] [bs '()])
    (cond
      [(null? as) (values #f xs)]
      [(f (car as)) (values (car as) (append (cdr as) bs))]
      [else (loop (cdr as) (cons (car as) bs))])))

#| Goal = State → Stream State |#
#| Goal1 = Promise+ Goal |#
#| Goal3 = ((succeed : Goal1) ⨯ (fail : Goal1)) |#

#| Goal → Goal1 |#
(define-syntax-rule (goal1 g) (delay/name g))

#| Goal1 → Goal1 → Goal3 |#
(define goal3 cons)

#| Goal3 → Goal1 |#
(define goal3-s car)
(define goal3-u cdr)

#| Goal3 → Goal3 |#
(define (noto g)
  (goal3 (goal3-s g) (goal3-u g)))

#| Goal3 → State → Stream State |#
(define (run-goal3 g s) ((force (car g)) s))

#| Goal → Goal → Goal |#
(define ((disj- g1 g2) s) (mplus (g1 s) (g2 s)))
(define ((conj- g1 g2) s) (bind (g1 s) g2))

#| Goal1 → Goal |#
(define ((goal1->goal g) s) (fmap g ($ s)))

#| Pos → [Promise+ a] → (Pos → [a] → [Promise+ a] → b) → b |#
(define (fmap-n n xs f)
  (let loop ([n n] [xs xs] [ys '()] [zs '()])
    (cond
      [(zero? n) (let-values ([(xs1 zs1) (partition promise? (append ys xs))])
                   (f 0 (append zs1 zs) xs1))]
      [(null? xs) (if (null? ys)
                      (f n zs '())
                      (loop n ys '() zs))]
      [(promise? (car xs)) (fmap-1 (car xs) (λ (a) (loop (pred n) (cdr xs) (cons a ys) zs)))]
      [else (loop n (cdr xs) ys (cons (car xs) zs))])))

#| (Goal → Goal → Goal) → ([Goal1] → Goal1) |#
(define ((lift1+ f) gs) (fmap-n max-size gs (λ (m gs gps) (lift1+- f gs gps))))
#| (Goal → Goal → Goal) → [Goal] → [Goal1] → Goal1 |#
(define (lift1+- f gs gps)
  (let loop ([gs gs] [gps gps])
    (if (null? gs)
        (if (null? gps)
            succeed-
            (sized (length gps) (let-values ([(gs1 gps1) (partition promise? (map force gps))]) (loop gs1 gps1))))
        (f (car gs) (goal1->goal (loop (cdr gs) gps))))))

#| [Goal1] → Goal1 |#
(define disj1+ (lift1+ disj-))
(define conj1+ (lift1+ conj-))

#| Promise Goal3 → Goal1 |#
(define (pgoal3-s g) (delay/name (force (goal3-s (force g)))))
(define (pgoal3-u g) (delay/name (force (goal3-u (force g)))))

#| ([Goal1] → Goal1) → ([Promise Goal3] → Goal3) |#
(define ((lift3+ f) gs) (goal3 (delay/name (force (f (map pgoal3-s gs)))) (delay/name (force (f (map pgoal3-u gs))))))

#| [Promise Goal3] → Goal3 |#
(define disj3+ (lift3+ disj1+))
(define conj3+ (lift3+ conj1+))

#| (s : Hash Var Any) → (d : Hash Var [Any]) → (c : [Constraint]) → (v : Nat) → State |#
(struct state (s d c v))

#| State |#
(define empty-state (state (make-immutable-hash) (make-immutable-hash) '() 0))

#| Id → Goal1 → Goal1 |#
(define-syntax-rule (fresh1 x g)
  (let ([x nothing])
    (fmap g (λ (g1)
              (λ (s)
                (let ([v (state-v s)])
                  (set! x (var v))
                  (g1 (state (state-s s) (state-d s) (state-c s) (+ 1 v)))))))))
#| (Var → Goal1) → Goal1 |#
;BUG
(define (call/fresh1 f)
  (goal1 (λ (s) (let ([v (state-v s)])
                  ((force (f (var v)))
                   (state (state-s s) (state-d s) (state-c s) (+ 1 v)))))))

#| (Var → Goal3) → Goal3 |#
(define (call/fresh f)
  (goal3 (goal1 (call/fresh1 (λ (v) (goal3-s (f v)))))
         (goal1 (call/fresh1 (λ (v) (goal3-u (f v)))))))

#| Goal3 ... → Goal3 |#
(define-syntax all
  (syntax-rules ()
    ((_ g) g)
    ((_ g ...) (conj3+ (list (delay g) ...)))))

#| Goal3 ... → Goal3 |#
(define disj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g ...) (disj3+ (list (delay g) ...)))))

(define-syntax-rule (conde (g0 g ...) ...) (disj+ (all g0 g ...) ...))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (all g0 g ...))
    ((_ (x0 x ...) g0 g ...) (call/fresh (λ (x0) (fresh (x ...) g0 g ...))))))

#| a → Hash a a → a |#
(define (walk x h) (hash-ref h x x))

#| a → Hash a a → a |#
(define (walk* x h)
  (let ([x (walk x h)])
    (if (pair? x)
        (cons (walk* (car x) h) (walk* (cdr x) h))
        x)))

#| Goal |#
(define succeed- (λ (s) (stream+ s)))

#| Goal3 |#
(define succeed (goal3 (goal1 succeed-) (goal1 (λ (s) '()))))

#| Goal3 |#
(define fail (noto succeed))

#| a → b → Goal1 |#
(define (==1 x y)
  (goal1
   (λ (s)
     (maybe->stream (unify x y s)))))

(define (== x y) (error '==))

#| (State → Maybe State) → Symbol → [Any] → [Var] → Constraint |#
(struct constraint (add kind parm vars))

#| [Var] → State → Maybe State |#
(define (check-fd vs s)
  (if (ormap
       (λ (v)
         (equal? #f (do bind-maybe+
                      d <- (hash-ref (state-d s) v nothing)
                      w <- (hash-ref (state-s s) v nothing)
                      (member (walk* w (state-s s)) (map (λ (x) (walk* x (state-s s))) d))))) vs)
      #f
      s))
#| [Var] → State → Maybe State |#
(define (check-constraints vs s)
  (do bind-maybe
    ns <- (check-constraints- vs s)
    (check-fd vs ns)))
(define (check-constraints- vs s)
  (let-values ([(ca cb) (partition (λ (c) (ormap (λ (x) (member x (constraint-vars c))) vs)) (state-c s))])
    (let loop ([cs ca] [s (state (state-s s) (state-d s) cb (state-v s))])
      (cond
        ((null? cs) s)
        (((constraint-add (car cs)) s) => (λ (s) (loop (cdr cs) s)))
        (else #f)))))

#| Maybe a → Stream a |#
(define (maybe->stream x)
  (cond
    [x (stream+ x)]
    [else '()]))

#| (Any ... → State → Maybe State) → [Var] → Any ... → Constraint |#
(define-syntax-rule (new-constraints op vs arg ...) (build-aux-oc op (arg ...) () (arg ...) vs))
(define-syntax build-aux-oc
  (syntax-rules ()
    ((_ op () (z ...) (arg ...) (v ...)) (let ([z arg] ...) (constraint (λ (s) (op z ... s)) `op (z ...) (list v ...))))
    ((_ op (x arg ...) (z ...) args vs) (build-aux-oc op (arg ...) (z ... n) args vs))))

#| a → a → Hash a a → Bool |#
(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(and (var? x) (var? v) (equal? x v)) #t]
      [(pair? v) (or (occurs? x (car v) s)
                     (occurs? x (cdr v) s))]
      [else #f])))

#| Var → a → State → Maybe State |#
(define (ext-s v x s)
  (let ([h (state-s s)])
    (cond
      ((occurs? v x h) #f)
      ((hash-has-key? h v) (error 'ext-s))
      (else (check-constraints (list v) (state (hash-set h v x) (state-d s) (state-c s) (state-v s)))))))

#| a → a → State → Maybe State |#
(define (unify u v s)
  (let ([u (walk u (state-s s))] [v (walk v (state-s s))])
    (cond
      ((equal? u v) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ([s (unify (car u) (car v) s)])
         (and s (unify (cdr u) (cdr v) s))))
      (else #f))))
