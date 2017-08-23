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
 new-id
 check-states
 pass
 pass+
 define-relation
 conj+
 disj+
 pass*
 pass*+
 call/fresh
 all
 conde
 fresh
 )
(require "state.rkt")
(require "stream.rkt")
(require "goal.rkt")
(require "id.rkt")

(define-state-cleaner s
  (state (remove-duplicates (state-g s)) (state-c s)))

(define-state-cleaner s
  (state (state-g s) (hash-map+filter-flip (state-c s)
                                           (λ (id cs)
                                             (let ([ncs (remove-duplicates cs)])
                                               (if (null? ncs)
                                                   #f
                                                   ncs))))))

#| Stream State → Stream State |#
(define (check-states ss) (stream-filter check-constraints ss))

#| State → Stream State |#
(define (pass s)
  (let ([g (state-g s)] [c (state-c s)])
    (if (null? g)
        s
        (stream-filter check-constraints (stream-map clean-state (patch+ (state '() c) (map run-goal g)))))))
(define (pass* s)
  (if (null? (state-g s))
      s
      (pass* (pass s))))

#| Stream State → Stream State |#
(define (pass+ ss) (stream-bind ss pass))
(define (pass*+ ss) (stream-bind ss pass*))

(define-syntax-rule (define-relation (name args ...) body)
  (let ([id (new-id)])
      (define (name args ...) (new-dgoal id (list args ...) (run-goal body)))))

#| [U Constraint Goal] → Goal |#
(define (conj+ gs)
  (new-agoal
   (let loop ([gs gs] [g '()] [c '()])
     (cond
       [(null? gs) (state-patch (values g c))]
       [(constraint? (car gs)) (loop (cdr gs) (cons (car gs) g) c)]
       [else (loop (cdr gs) g (cons (car gs) c))]))))
(define (disj+ gs)
  (new-agoal
   (let loop ([gs gs] [rs '()])
     (if (null? gs)
         (state-patch rs)
         (loop (cdr gs) (cons
                         (if (constraint? (car gs))
                             (values '() (list (car gs)))
                             (values (list (car gs)) '()))
                         rs))))))

#| (Var → U Constraint Goal) → U Constraint Goal |#
(define (call/fresh f) (f (new-var)))

(define (all . gs) (conj+ gs))
(define-syntax-rule (conde ((g ...) ...)) (disj+ (list (all g ...) ...)))
(define-syntax fresh
  (syntax-rules ()
    [(_ () (g ...)) (all g ...)]
    [(_ (x0 x ...) gs) (call/fresh (λ (x0) (fresh (x ...) gs)))]))
