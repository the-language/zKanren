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
(provide
 (struct-out var)
 pass
 pass+
 pass*
 pass*+
 goalf->dgoalf
 define-relation
 conj+-
 conj+
 disj+-
 disj+
 call/fresh
 all
 conde
 fresh
 (struct-out constraints)
 (struct-out constraint)
 new-constraints
 new-constraint
 define-constraints-
 define-constraints
 (struct-out state)
 (struct-out state-patch)
 define-state-cleaner-
 define-state-cleaner
 )
(require "constraint.rkt")
(require "state.rkt")
(require "stream.rkt")
(require "goal.rkt")
(require "id.rkt")

;(define-state-cleaner s
;  (state (remove-duplicates (state-g s)) (state-c s)))

#| SizedStream State → SizedStream State |#
(define (check-states ss) (sizedstream-filter check-constraints ss))

#| State → SizedStream State |#
(define (pass s)
  (let ([g (state-g s)] [c (state-c s)])
    (if (null? g)
        (sizedstream s)
        (check-states (stream-map clean-state (patch/check+ (state '() c) (map run-goal g)))))))
(define (pass* s)
  (if (null? (state-g s))
      (sizedstream s)
      (sizedstream-bind (pass s) pass*)))

#| SizedStream State → SizedStream State |#
(define (pass+ ss) (sizedstream-bind ss pass))
(define (pass*+ ss) (sizedstream-bind ss pass*))

#| (... → Goal) → (... → DGoal) |#
(define ((goalf->dgoalf f) . args) (new-dgoal (new-id) args (run-goal (apply f args))))

(define-syntax-rule (define-relation (name args ...) body)
  (define name (goalf->dgoalf (λ (args ...) body))))

#| [U Constraint Goal] → Goal |#
(define (conj+- gs)
  (new-agoal
   (let loop ([gs gs] [g '()] [c '()])
     (cond
       [(null? gs) (state-patch (values g c))]
       [(constraint? (car gs)) (loop (cdr gs) (cons (car gs) g) c)]
       [else (loop (cdr gs) g (cons (car gs) c))]))))
(define (disj+- gs)
  (new-agoal
   (let loop ([gs gs] [rs '()])
     (if (null? gs)
         (state-patch rs)
         (loop (cdr gs) (cons
                         (if (constraint? (car gs))
                             (values '() (list (car gs)))
                             (values (list (car gs)) '()))
                         rs))))))

#| ([U Constraint Goal] → Goal) → ([Goal+] → Goal+) |#
(define ((lift+ f) gs) (goal+ (f (map goal+-s gs)) (f (map goal+-u gs))))

#| [Goal+] → Goal+ |#
(define conj+ (lift+ conj+-))
(define disj+ (lift+ disj+-))

#| (Var → a) → a |#
(define (call/fresh f) (f (new-var)))

(define (all . gs) (conj+ gs))
(define-syntax-rule (conde (g0 g ...) (g0* g* ...) ...)
  (disj+ (list (all g0 g ...) (all g0* g* ...) ...)))
(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (all g ...)]
    [(_ (x0 x ...) g ...) (call/fresh (λ (x0) (fresh (x ...) g ...)))]))
