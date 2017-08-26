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
 ==c
 ==
 )
(require "../zk.rkt")

#| ConstraintsV = Hash Var Any |#
#| ConstraintV = Any × Any |#

#| a → Hash a a → a |#
(define (walk x h) (hash-ref x h x))

#| ConstraintsV → Any → Any → Maybe [Var × Any] |#
(define (unify cv x y)
  (let ([x (walk x cv)] [y (walk y cv)])
    (cond
      [(equal? x y) '()]
      [(var? x) (list (cons x y))]
      [(var? y) (list (cons y x))]
      [(and (pair? x) (pair? y)) (let ([xs (unify cv (car x) (car y))] [ys (unify cv (cdr x) (cdr y))])
                                   (and xs ys (append xs ys)))]
      [(and (vector? x) (vector? y)) (unify cv (vector->list x) (vector->list y))]
      [(and (struct? x) (struct? y)) (unify cv (struct->vector x) (struct->vector y))]
      [else #f])))

#| Any → Any → Constraint |#
(define (==- x y) (new-constraint ==c (cons x y)))

#| Any → Any → Goal+ |#
(define (== x y) (goal+ (==- x y) (==- x y)));;u:BUG

(define-constraints ==c
  (hash)
  (λ (cv s)
    (let ([x (car cv)] [y (cdr cv)])
      (let* ([csv (get-constraintsv s ==c)] [nc (unify csv x y)])
        (and nc (let loop ([csv csv] [nc nc] [vs '()])
                  (if (null? nc)
                      (cons (set-constraintsv s ==c csv) vs)
                      (let ([a (car nc)])
                        (let ([v (car a)] [x (cdr a)])
                          (loop (hash-set csv v x) (cdr nc) (cons v vs))))))))))
  (λ (vs s) #t)
  (λ (s) #f)
  (λ (s) (cons '== (get-constraintsv s ==c))))
