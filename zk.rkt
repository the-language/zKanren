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

#| Nat → Var |#
(struct var (v))

#| Positive-Integer → Promise a → Sized a |#
(struct sized (s v))

#| Positive-Integer → a → Sized a |#
(define-syntax-rule (new-sized s e)
  (sized s (delay/name e)))

#| Stream a = U () (a, (Stream a)) (Promise (Stream a)) |#
#| a → Stream a → Stream a |#
(define (stream-cons a d) (delay/name (cons a d)))
#| Stream a → (a, Stream a) |#
(define (pull xs) (if (promise? xs) (pull (force xs)) xs))
#| Positive-Integer → Stream a → [a] |#
(define (take-stream n xs)
  (if (zero? n)
      '()
      (let ([xs (pull xs)])
        (if (null? xs)
            '()
            (cons (car xs) (take (- n 1) (cdr xs)))))))
#| Stream a → [a] |#
(define (force-stream xs)
  (let ([xs (pull xs)])
    (if (null? xs)
        '()
        (cons (car xs) (force-stream (cdr xs))))))


#| (Sized Goal) → Goal0 |#
(struct goal0 (v))

#| Positive-Integer → Goal → Goal0 |#
(define-syntax-rule (new-goal0 s g)
  (goal0 (new-sized s g)))

#| Goal0 → Goal0 → Bool |#
(define (>goal0 x y)
  (> (sized-s (goal0-v x)) (sized-s (goal0-v y))))

#| [Goal0] → ConjV |#
(struct conj-v (v))

#| [Goal0] → SDisjV |#
(struct sdisj-v (v))
#| Goal0 → Promise Goal2 → LDisjV |#
(struct ldisj-v (h t))
(define disj-v-max 16)
#| (x : t) → ((if elem t [SDisjV, LDisjV] then True else False) : Bool) |#
(define (disj-v? x) (or (sdisj-v? x) (ldisj-v? x)))
#| DisjV = U SDisjV LDisjV |#

#| SDisjV → DisjV |#
(define (sdisj-v-check g)
  (if (> (length (sdisj-v-v g)) disj-v-max)
      (let-values ([(h t) (split-at (sdisj-v-v g) disj-v-max)])
        (ldisj-v (sdisj-v->goal0 (sdisj-v h)) (goal1->goal2 (sdisj-v t))))
      g))

#| [Goal0] → DisjV |#
(define (sdisj-v-c gs)
  (sdisj-v-check (sdisj-v gs)))

#| Goal = State → Stream State |#
#| Goal1 = U Goal0 ConjV DisjV |#
#| Goal2 = State → Promise (State, Goal1) |#
#| Goal3 = ((succeed : Promise Goal2), (fail : Promise Goal2)) |#

#| Goal3 → Goal3 |#
(define (noto g)
  (cons (cdr g) (car g)))

#| Goal1 → Goal2 |#
(define-syntax-rule (goal1->goal2 g) (λ (s) (delay/name (cons s g))))

#| Stream a → Stream a → Stream a |#
(define (mplus xs ys)
  (cond
    ((null? xs) ys)
    ((promise? xs) (delay/name (mplus ys (force xs))))
    (else (cons (car xs) (mplus ys (cdr xs))))))

#| Stream a → (a → Stream b) → Stream b |#
(define (bind xs f)
  (cond
    ((null? xs) '())
    ((promise? xs) (delay/name (bind (force xs) f)))
    (else (mplus (f (car xs)) (bind (cdr xs) f)))))

#| Goal0 → Goal0 → Goal0 |#
(define (conj0 g1 g2)
  (let ([g1 (goal0-v g1)] [g2 (goal0-v g2)])
    (new-goal0 (+ (sized-s g1) (sized-s g2)) (λ (s) (bind ((force (sized-v g1)) s) (force (sized-v g2)))))))

#| DisjV → Goal0 |#
(define (disj-v->goal0 g)
  (if (sdisj-v? g)
      (sdisj-v->goal0 g)
      (ldisj-v->goal0 g)))

#| Goal0 → Goal0 → Goal0 |#
(define (disj0 g1 g2)
  (let ([g1 (goal0-v g1)] [g2 (goal0-v g2)])
    (new-goal0 (+ (sized-s g1) (sized-s g2)) (λ (s) (mplus ((force (sized-v g1)) s) ((force (sized-v g2)) s))))))

#| Goal1 → Goal1 → ConjV |#
(define (conj1 g1 g2)
  (cond
    ((disj-v? g1) (conj1 (disj-v->goal0 g1) g2))
    ((disj-v? g2) (conj1 g2 g1))
    ((conj-v? g1) (if (conj-v? g2)
                      (conj-v (append (conj-v-v g1) (conj-v-v g2)))
                      (conj-v (cons g2 (conj-v-v g1)))))
    ((conj-v? g2) (conj-v (cons g1 (conj-v-v g2))))
    (else (conj-v (list g1 g2)))))

#| Promise Goal0 → DisjV → DisjV |#
(define (cons-disj g d)
  (if (sdisj-v? d)
      (sdisj-v-c (cons (force g) (sdisj-v-v d)))
      (ldisj-v (ldisj-v-h d) (disj2 (goal1->goal2 (force g)) (ldisj-v-t d)))))

#| DisjV → Promise DisjV → DisjV |#
(define (append-disj-v d pd)
  (if (sdisj-v? d)
      (let ([pd (force pd)])
        (if (sdisj-v? pd)
            (sdisj-v-c (append (sdisj-v-v d) (sdisj-v-v pd)))
            (ldisj-v (ldisj-v-h pd) (delay/name (disj2 (goal1->goal2 d) (ldisj-v-t pd))))))
      (ldisj-v (ldisj-v-h d) (delay/name (disj2 (goal1->goal2 d) pd)))))

#| Promise Goal1 → DisjV → DisjV |#
(define (cons-disj1 g d)
  (if (sdisj-v? d)
      (let ([g (force g)])
        (cond
          ((conj-v? g) (sdisj-v-c (cons (conj-v->goal0 g) (sdisj-v-v d))))
          ((disj-v? g) (append-disj-v d (delay/name g)))
          (else (sdisj-v-c (cons g (sdisj-v-v d))))))
      (ldisj-v (ldisj-v-h d) (disj2 (goal1->goal2 g) (ldisj-v-t d)))))

#| Goal1 → Promise Goal1 → DisjV |#
(define (disj1 g1 g2)
  (cond
    ((conj-v? g1) (disj1 (conj-v->goal0 g1) g2))
    ((disj-v? g1) (cons-disj1 g2 g1))
    (else (let ([g2 (force g2)])
            (cond
              ((conj-v? g2) (sdisj-v-c (list g1 (conj-v->goal0 g2))))
              ((disj-v? g2) (cons-disj (delay/name g1) g2))
              (else (sdisj-v-c (list g1 g2))))))))

#| ConjV → Goal0 |#
(define (conj-v->goal0 g)
  (let ([gs (sort (conj-v-v g) >goal0)])
    (foldl conj0 (car gs) (cdr gs))))

#| SDisjV → Goal0 |#
(define (sdisj-v->goal0 g)
  (let ([gs (sort (sdisj-v-v g) >goal0)])
    (foldl disj0 (car gs) (cdr gs))))

#| LDisjV → Goal0 |#
(define (ldisj-v->goal0 g)
  (let ([h (ldisj-v-h g)])
    (goal0 (sized (sized-s (goal0-v h))
                  (delay/name
                   (λ (s)
                     (mplus (delay/name ((force (sized-v (goal0-v h))) s))
                            (delay/name ((goal2->goal (force (ldisj-v-t g))) s)))))))))

#| Goal1 → Goal0 |#
(define (goal1->goal0 g)
  (cond
    ((disj-v? g) (disj-v->goal0 g))
    ((conj-v? g) (conj-v->goal0 g))
    (else g)))

#| Goal2 → Goal |#
(define ((goal2->goal g) s)
  (delay/name (delay/name (delay/name
                           (let ([gr (force (g s))])
                             (let ([s (car gr)] [g (cdr gr)])
                               ((force (sized-v (goal0-v (goal1->goal0 g)))) s)))))))

#| (Goal1 → Goal1 → Goal1) → (Goal2 → Goal2 → Goal2) |#
(define (((liftgoal1->goal2 f) g1 g2) s)
  (delay/name (let ([g1r (force (g1 s))])
                (let ([g2r (force (g2 (car g1r)))] [g1 (cdr g1r)])
                  (let ([g2 (cdr g2r)] [s (car g2r)])
                    (cons s (f g1 g2)))))))

#| Goal2 → Goal2 → Goal2 |#
(define conj2 (liftgoal1->goal2 conj1))

#| Goal2 → Promise Goal2 → Goal2 |#
(define ((disj2 g1 g2) s)
  (delay/name (let ([g1r (force (g1 s))])
                (let ([s (car g1r)] [g1 (cdr g1r)])
                  (cond
                    ((conj-v? g1) (let ([g2r (force ((force g2) s))])
                                    (let ([s (car g2r)] [g2 (cdr g2r)])
                                      (cons s (disj1 g2 (delay/name (conj-v->goal0 g1)))))))
                    ((sdisj-v? g1) (let ([g2r (force ((force g2) s))])
                                     (let ([s (car g2r)] [g2 (cdr g2r)])
                                       (cons s (disj1 g1 (delay/name g2))))))
                    ((ldisj-v? g1) (cons s (disj1 g1 (delay/name (goal0 (sized 9 (delay/name (goal2->goal (force g2)))))))))
                    (else (let ([g2r (force ((force g2) s))])
                            (let ([s (car g2r)] [g2 (cdr g2r)])
                              (cons s (disj1 g2 (delay/name g1)))))))))))

#| Goal3 → Promise Goal3 → Goal3 |#
(define (disj g1 g2)
  (cons (delay/name (disj2 (force (car g1)) (delay/name (force (car (force g2))))))
        (delay/name (conj2 (force (cdr g1)) (force (cdr (force g2)))))))

#| Goal3 → Promise Goal3 → Goal3 |#
(define (conj g1 g2) (noto (disj g1 g2)))

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
