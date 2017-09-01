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
 t
 leto/all
 leto)
(require "unify.rkt")

(define-syntax t
  (syntax-rules ()
    [(_ (f x ...) v) (applyo f (x ...) v)]
    [(_ x y)
     (if (procedure? x)
         (x y)
         (== x y))]))

(define-syntax-rule (applyo f (x ...) v)
  (%applyo f (x ...) () v))

(define-syntax %applyo
  (syntax-rules ()
    [(_ f (x0 x ...) (v ...) r)
     (fresh (v1)
            (t x0 v1)
            (%applyo f (x ...) (v ... v0) r))]
    [(_ f () (v ...) r) (f v ... r)]))


(define-syntax-rule (leto/all ([x v] ...) b)
  (fresh (x ...)
         (t v x) ...
         b))

(define-syntax-rule (leto ([x v] ...) y r)
  (leto/all ([x v] ...)
            (t y r)))
