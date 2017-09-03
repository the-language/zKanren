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
 λr
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
 runzk-
 runzk
 (struct-out goal+)
 noto
 struct
 succeed
 fail
 )
(require "constraint.rkt")
(require "state.rkt")
(require "stream.rkt")
(require "goal.rkt")
(require "id.rkt")
(require "struct.rkt")
(require "memorize.rkt")

(define-state-cleaner s
  (let ([gs (remove-duplicates (state-g s))])
    (if (< (length gs) (length (state-g s)))
        (state gs (state-c s) (state-hg s))
        #f)))

;(define-state-cleaner s
;  (let ([gs (filter-not (λ (x) (set-member? (state-hg s) x)) (remove-duplicates (state-g s)))])
;    (if (< (length gs) (length (state-g s)))
;        (state gs (state-c s) (state-hg s))
;        #f)))

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

(define/contract (run- g)
  (-> (or/c constraint? goal?) (sizedstream/c state?))
  (pass* (new-state+ g)))

(define/contract (run-+ g)
  (-> goal+? (sizedstream/c state?))
  (run- (goal+-s g)))
(define/contract (runzk- g)
  (-> goal+? (sizedstream/c (listof (cons/c symbol? list?))))
  (sizedstream-map (λ (s) (hash-map (state-c s) (λ (id v) ((constraints-show (get-constraints- id)) s)))) (run-+ g)))
(define-syntax-rule (runzk (v ...) g ...)
  (let ([v (new-var)] ...)
    (cons (list v ...) (runzk- (all g ...)))))
(define-syntax-rule (runzk* (v ...) g ...)
  (let ([x (runzk (v ...) g ...)])
    (cond (car x) (sizedstream->list (cdr x)))))

#| Goal → Goal |#
(define-syntax-rule (goal-pack g) (new-goal (state-patch (list (state-patch1 (list g) '())))))

#| Promise Goal+ → Goal+ |#
(define (recgoal+ g)
  (goal+ (goal-pack (->goal (goal+-s (force g))))
         (goal-pack (->goal (goal+-u (force g))))))

(define-syntax-rule (λr args body)
  (memorize (λ args (recgoal+ (delay body)))))

(define-syntax-rule (define-relation (name arg ...) body)
  (define name (λr (arg ...) body)))

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
(define-syntax-rule (fresh (v ...) g ...)
  (let ([v (new-var)] ...)
    (all g ...)))

(define-constraints failc
  'n
  (λ (x s) #f)
  (λ (vs s) #f)
  (λ (s) #f)
  (λ (s) (error 'failc)))

(define fail- (new-constraint failc 'c))
(define succeed- (new-goal (state-patch (list (state-patch1 '() '())))))

#| Goal+ |#
(define succeed (goal+ succeed- fail-))
(define fail (goal+ fail- succeed-))
