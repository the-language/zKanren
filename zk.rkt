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
 var?
 pass-
 pass
 pass*
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
 define-state-cleaner-
 define-state-cleaner
 get-constraintsv
 set-constraintsv
 new-state
 run-
 run-+
 runzk
 (struct-out goal+)
 noto
 struct
 )
(require "constraint.rkt")
(require "state.rkt")
(require "stream.rkt")
(require "goal.rkt")
(require "id.rkt")
(require "struct.rkt")

;(define-state-cleaner s
;  (state (remove-duplicates (state-g s)) (state-c s)))

#| State → SizedStream State |#
(define (pass- s)
  (let ([g (state-g s)] [c (state-c s)])
    (if (null? g)
        (sizedstream s)
        (patch+ (state '() c (state-hg s)) (map run-goal g)))))
(define (pass s)
  (sizedstream-map clean-state (pass- s)))
(define (pass* s)
  (if (null? (state-g s))
      (sizedstream s)
      (sizedstream-bind (pass s) pass*)))

#| U Constraint Goal → SizedStream State |#
(define/contract (run- g)
  (-> (or/c constraint? goal?) (sizedstream/c state?))
  (pass* (new-state+ g)))

#| Goal+ → SizedStream (Hash ID ConstraintsV) |#
(define/contract (run-+ g)
  (-> goal+? (sizedstream/c state?))
  (run- (goal+-s g)))
#| Goal+ → SizedStream [Symbol × [Any]] |#
(define/contract (runzk g)
  (-> goal+? (sizedstream/c (listof (cons/c symbol? list?))))
  (sizedstream-map (λ (s) (hash-map (state-c s) (λ (id v) ((constraints-show (get-constraints- id)) s)))) (run-+ g)))

(define-syntax-rule (define-relation (name . args) body)
  (define name (memorize (λ args body))))

#| U Constraint Goal → Goal |#
(define (->goal x)
  (if (constraint? x)
      (new-goal (state-patch (list (state-patch1 '() (list x)))))
      x))

#| [U Constraint Goal] → Goal |#
(define (conj+- gs)
  (new-goal
   (let loop ([gs gs] [g '()] [c '()])
     (cond
       [(null? gs) (state-patch (list (state-patch1 g c)))]
       [(constraint? (car gs)) (loop (cdr gs) g (cons (car gs) c))]
       [else (loop (cdr gs) (cons (car gs) g) c)]))))
(define (disj+- gs)
  (new-goal
   (let loop ([gs gs] [rs '()])
     (if (null? gs)
         (state-patch rs)
         (loop (cdr gs) (cons
                         (if (constraint? (car gs))
                             (state-patch1 '() (list (car gs)))
                             (state-patch1 (list (car gs)) '()))
                         rs))))))

#| ([U Constraint Goal] → Goal) → ([Goal+] → Goal+) |#
(define/contract ((lift+ f) gs)
  (((listof (or/c constraint? goal?)) . -> . goal?) . -> . ((listof goal+?) . -> . goal+?))
  (goal+ (f (map goal+-s gs)) (f (map goal+-u gs))))

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
