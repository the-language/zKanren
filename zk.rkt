;;  zKanren: MicroKanren with Constraints and Optimizations
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
(require "stream.rkt")
(provide (all-defined-out))

(define max-size 32)

#| Nat → Var |#
(struct var (v))

#| Pos = Positive-Integer |#

#| Nat → a → Promise+ a |#
(define-syntax-rule (sized n x) (raw-sized n (delay/name x)))
(define (raw-sized n x) (if (zero? n) (force x) (delay/name (sized (- n 1) x))))

#| Promise+ a = U a (Promise (Promise+ a)) |#

#| Promise+ a → a |#
(define (force+ x) (if (promise? x) (force+ (force x)) x))

#| Promise+ a → (a → b) → Promise+ b |#
(define (fmap x f) (if (promise? x) (delay/name (fmap (force x) f)) (f x)))

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

#| (Goal → Goal → Goal) → ([Goal1] → Goal1) |#
(define ((lift1+ f) gs)
  (let-values ([(h t) (find-a gs (λ (x) (not (promise? x))))])
    (if h
        (f h (goal1->goal (delay/name (force ((lift1+ f) t)))))
        (sized (length gs) (disj1+ (map force gs))))))

#| [Goal1] → Goal1 |#
(define disj1+ (lift1+ disj-))
(define conj1+ (lift1+ conj-))

#| Goal3 → Promise Goal3 → Goal3 |#
(define (conj g1 g2) (goal3 (conj1 (goal3-s g1) (goal1 (force (goal3-s (force g2)))))
                            (disj1 (goal3-s g1) (goal1 (force (goal3-s (force g2)))))))
(define (disj g1 g2) (goal3 (disj1 (goal3-s g1) (goal1 (force (goal3-s (force g2)))))
                            (conj1 (goal3-s g1) (goal1 (force (goal3-s (force g2)))))))


#| (s : Hash Var Any) → (d : Hash Var [Any]) → (c : [Constraint]) → (v : Nat) → State |#
(struct state (s d c v))

#| State |#
(define empty-state (state (make-immutable-hash) (make-immutable-hash) '() 0))

#| (Var → Goal1) → Goal1 |#
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
    ((_ g0 g ...) (conj g0 (delay (all g ...))))))

#| Goal3 ... → Goal3 |#
(define disj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (disj g0 (delay (all g ...))))))

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

#| Goal3 |#
(define succeed (goal3 (goal1 (λ (s) (stream+ s))) (goal1 (λ (s) '()))))

#| Goal3 |#
(define fail (noto succeed))

(define (== x y) (error '==))

#| (State → Maybe State) → Symbol → [Any] → [Var] → Constraint |#
(struct constraint (add kind parm vars))

#| [Var] → State → Maybe State |#
(define (check-constraints vs s)
  (let-values ([(ca cb) (partition (λ (c) (ormap (λ (x) (member x (constraint-vars c))) vs)) (state-c s))])
    (let loop ([cs ca] [s (state (state-s s) (state-d s) cb (state-v s))])
      (cond
        ((null? cs) s)
        (((constraint-add (car cs)) s) => (λ (s) (loop (cdr cs) s)))
        (else #f)))))

#| [Var] → State → U () (Promise (State ⨯ ())) |#
(define (check-constraints-stream vs s)
  (cond
    ((check-constraints-stream vs s) => (λ (s) (stream+ s)))
    (else '())))

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
      ((equal? x v) #t)
      ((pair? v) (or (occurs? x (car v) s)
                     (occurs? x (cdr v) s)))
      (else #f))))

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
