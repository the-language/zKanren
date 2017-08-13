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

#| a → a |#
(define (id x) x)

#| Nat → Var |#
(struct var (v))

#| Positive-Integer → Promise a → Sized a |#
(struct sized (s v))

#| Stream a = U Null (a, (Stream a)) (Promise (Stream a)) |#

#| Stream a |#
(define stream-nil (delay/name '()))

#| a → Stream a → Stream a |#
(define (stream-cons a d) (delay/name (cons a d)))

#| (Sized (State → Stream State)) → Goal0 |#
(struct goal0 (v))

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

#| Goal1 = U Goal0 ConjV DisjV |#
#| Goal2 = State → Promise (State, Goal1) |#
#| Goal3 = ((succeed : Goal2), (fail : Goal2)) |#

#| Stream a → Stream a → Stream a |#
(define (mplus xs ys)
  (cond
    ((null? xs) ys)
    ((promise? xs) (delay/name (mplus ys (force xs))))
    (else (cons (car xs) (mplus (cdr xs) ys)))))

#| Stream a → (a → Stream b) → Stream b |#
(define (bind xs f)
  (cond
    ((null? xs) '())
    ((promise? xs) (delay/name (bind (force xs) f)))
    (else (mplus (f (car xs)) (bind (cdr xs) f)))))

#| Goal1 → Goal1 → ConjV |#
(define (conj g1 g2)
  (cond
    ((disj-v? g1) (conj (disj-v->goal0 g1) g2))
    ((disj-v? g2) (conj g2 g1))
    ((conj-v? g1) (if (conj-v? g2)
                      (conj-v (append (conj-v-v g1) (conj-v-v g2)))
                      (conj-v (cons g2 (conj-v-v g1)))))
    ((conj-v? g2) (conj-v (cons g1 (conj-v-v g2))))
    (else (conj-v (list g1 g2)))))

#| Goal1 → Goal0 |#
(define (goal1->goal0 g)
  (cond
    ((disj-v? g) (disj-v->goal0 g))
    ((conj-v? g) (conj-v->goal0 g))
    (else g)))

#| Goal0 → Goal0 → Bool |#
(define (>goal0 x y)
  (> (sized-s (goal0-v x)) (sized-s (goal0-v y))))

#| [Num] → Num |#
(define (sum xs) (foldl + 0 xs))

#| [Goal0] → Num |#
(define (sum-goal0 xs) (sum (map (λ (x) (sized-s (goal0-v x))) xs)))

#| ConjV → Goal0 |#
(define (conj-v->goal0 g)
  (let ((gs (sort (conj-v-v g) >goal0)))
    (goal0 (sized
            (sum-goal0 gs)
            (delay/name (foldl doconj-v->goal0 (car gs) (cdr gs)))))))

#| Goal0 → Goal0 → (State → Stream State) |#
(define ((doconj-v->goal0 g1 g2) s)
  (bind ((force (sized-v (goal0-v g1))) s)
        (force (sized-v (goal0-v g2)))))

#| Goal3 → Goal3 |#
(define (noto g)
  (cons (cdr g) (car g)))

#| State = ((s : Hash Var Any), (((d : Hash Var [Any]), (c : [Constraint]))) |#

#| (Goal1 → Goal1 → Goal1) → (Goal2 → Goal2 → Goal2) |#
(define (((liftgoal1->goal2 f) g1 g2) s)
  (delay/name (let ([g1r (force (g1 s))])
                (let ([g2r (force (g2 (car g1r)))] [g1 (cdr g1r)])
                  (let ([g2 (cdr g2r)] [s (car g2r)])
                    (cons s (f g1 g2)))))))

#| Goal2 → Goal2 → Goal2 |#
(define conj+ (liftgoal1->goal2 conj))

#| Goal2 → (State → Stream State) |#
(define ((goal2->goal g) s)
  (delay/name (delay/name (delay/name
                           (let ([gr (force (g s))])
                             (let ([s (car gr)] [g (cdr gr)])
                               ((force (sized-v (goal0-v (goal1->goal0 g)))) s)))))))

#| Goal0 → Goal0 → Goal0 |#
(define (disj- g1 g2)
  (let ([g1 (goal0-v g1)] [g2 (goal0-v g2)])
    (goal0 (+ (sized-s g1) (sized-s g2)) (delay/name (λ (s) (mplus ((force g1) s) ((force g2) s)))))))

#| SDisjV → Goal0 |#
(define (sdisj-v->goal0 g)
  (let ([gs (sort (sdisj-v-v g) >goal0)])
    (foldl disj- (car gs) (cdr gs))))

#| LDisjV → Goal0 |#
(define (ldisj-v->goal0 g)
  (let ([h (ldisj-v-h g)])
    (goal0 (sized (sized-s (goal0-v h))
                  (delay/name
                   (λ (s)
                     (mplus (delay/name ((force (sized-v (goal0-v h))) s))
                            (delay/name ((goal2->goal (force (ldisj-v-t g))) s)))))))))

#| DisjV → Goal0 |#
(define (disj-v->goal0 g)
  (if (sdisj-v? g)
      (sdisj-v->goal0 g)
      (ldisj-v->goal0 g)))
