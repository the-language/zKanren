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

#| Pos → Promise a → Sized a |#
(struct sized (s v))
#| Pos → a → Sized a |#
(define-syntax-rule (new-sized s e)
  (sized s (delay/name e)))
#| Sized a → a |#
(define (run-sized x) (force (sized-v x)))

#| (Sized Goal) → Goal0 |#
(struct goal0 (v))
#| Pos → Goal → Goal0 |#
(define-syntax-rule (new-goal0 s g)
  (goal0 (new-sized s g)))
#| Goal0 → Pos |#
(define (goal0-s g) (sized-s (goal0-v g)))
#| Goal0 → Goal |#
(define (run-goal0 g) (run-sized (goal0-v g)))
#| Goal0 → Goal0 → Bool |#
(define (>goal0 x y)
  (> (sized-s (goal0-v x)) (sized-s (goal0-v y))))

#| Goal2 → Promise Goal2 → Conj2 |#
(struct conj2 (h t))

#| Goal2 → Promise Goal2 → Disj2 |#
(struct disj2 (h t))

#| Goal = State → Stream State |#
#| Goal1 = U Goal0 Conj2 Disj2 |#
#| Goal1.5 = State → (Values State Goal0) |#
#| Goal2 = State → (Values State Goal1) |#
#| Goal3 = ((succeed : Promise Goal2) ⨯ (fail : Promise Goal2)) |#

#| [Goal0] → Maybe (Promise Goal2) → ConjE |#
(struct conje (h t))
#| [Goal0] → Maybe (Promise Goal2) → DisjE |#
(struct disje (h t))
#| Goal1E = U Goal0 ConjE DisjE |#
#| Goal2E = State → (Values State Goal1E) |#

#| Goal2 → Goal2 → Goal3 |#
(define-syntax-rule (new-goal3 s u) (cons (delay/name s) (delay/name u)))

#| Goal3 → Goal3 |#
(define (noto g)
  (cons (cdr g) (car g)))

#| Goal3 → Goal2 |#
(define (run-goal3 g) (force (car g)))

#| Goal1 → Goal2 |#
(define-syntax-rule (goal1->goal2 g) (λ (s) (values s g)))
#| Goal0 → Goal1.5 |#
(define-syntax-rule (goal0->goal1.5 g) (goal1->goal2 g))

#| Goal0 → Goal0 → Goal0 |#
(define (conj0 g1 g2)
    (new-goal0 (+ (goal0-s g1) (goal0-s g2))
               (λ (s) (bind ((run-goal0 g1) s) (run-goal0 g2)))))

#| Goal0 → Goal0 → Goal0 |#
(define (disj0 g1 g2)
    (new-goal0 (+ (goal0-s g1) (goal0-s g2))
               (λ (s) (mplus ((run-goal0 g1) s) ((run-goal0 g2) s)))))

#| Goal1.5 → Goal |#
(define ((goal1.5->goal g) s)
  (delay/name
   (let-values ([(s g) (g s)])
     ((run-goal0 g) s))))

#| Goal2 → Goal |#
(define ((goal2->goal g) s)
  (delay/name
   (let-values ([(s g) (g s)])
     ((goal1.5->goal (eval-goal1 max-size g)) s))))

#| (Goal0 → Goal0 → Goal0) → [Goal0] → Goal0 |#
(define (fold-goal0 f gs) (let ([gs (sort gs >goal0)]) (foldl f (car gs) (cdr gs))))

#| Pos → Goal2 → Goal1.5 |#
(define ((eval-goal2 local-max-size g) s)
  (let-values ([(s g) (g s)])
    (let-values ([(s g) ((eval-goal1 local-max-size g) s)])
      (values s g))))

#|
#| Pos → Goal1 → Goal1.5 |#
(define (eval-goal1 local-max-size g)
  (cond
    [(goal0? g) (goal0->goal1.5 g)]
    [(disj2? g)
     (λ (s)
       (let loop ([g (goal1->goal2 g)] [local-max-size local-max-size] [gs '()] [s s])
         (if (<= local-max-size 0)
             (values s (disj0 (fold-goal0 disj0 gs) (new-goal0 1 (goal2->goal g))))
             (let-values ([(s g) (g s)])
               (cond
                 [(goal0? g) (values s g)]
                 [(disj2? g) |#

#| Pos → Goal1 → Goal2E |#
(define (goal1->goal2e local-max-size g)
#| (s : Hash Var Any) → (d : Hash Var [Any]) → (c : [Constraint]) → (v : Nat) → State |#
(struct state (s d c v))

#| State |#
(define empty-state (state (make-immutable-hash) (make-immutable-hash) '() 0))

#| (Var → Goal2) → Goal2 |#
(define ((call/fresh2 f) s)
  (let ([v (state-v s)])
    ((f (var v)) (state (state-s s) (state-d s) (state-c s) (+ 1 v)))))

#| (Var → Goal3) → Goal3 |#
(define (call/fresh f)
  (cons (delay/name (call/fresh2 (λ (v) (force (car (f v))))))
        (delay/name (call/fresh2 (λ (v) (force (cdr (f v))))))))

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

#| Goal3 |#
(define succeed (new-goal3 (goal1->goal2 (new-goal0 1 (λ (s) (stream s))))
                           (goal1->goal2 (new-goal0 1 (λ (s) '())))))
#| Goal3 |#
(define fail (noto succeed))

(define == (error '==))

#| (State → Maybe State) → Symbol → [Any] → [Var] → Constraint |#
(struct constraint (add kind parm vars))

#| [Var] → State → Maybe State |#
(define (check-constraints vs s)
  (let-values ([(ca cb) (partition (λ (c) (ormap (λ (x) (member x (constraint-vars c))) vs)) (state-c s))])
    (let loop ([cs ca] [s (state (state-s s) (state-d s) cb)])
      (cond
        ((null? cs) s)
        (((constraint-add (car cs)) s) => (λ (s) (loop (cdr cs) s)))
        (else #f)))))

#| [Var] → State → U () (Promise (State ⨯ ())) |#
(define (check-constraints-stream vs s)
  (cond
    ((check-constraints-stream vs s) => (λ (s) (stream s)))
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
      (else (check-constraints (list v) (state (hash-set h v x) (state-d s) (state-c s)))))))

#| a → a → State → Maybe State |#
(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond
      ((equal? u v) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ([s (unify (car u) (car v) s)])
         (and s (unify (cdr u) (cdr v) s))))
      (else #f))))
