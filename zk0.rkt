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
(provide $ sized mplus bind succeed0 fail0 disj0 conj0)
(require "stream.rkt")

#| a → ((a → b) → b) |#
(define (($ x) f) (f x))

#| Nat → a → Promise+ a |#
(define-syntax-rule (sized n x) (raw-sized n (delay/name x)))
#| Nat → Promise a → Promise+ a |#
(define (raw-sized n x) (if (zero? n) (force x) (delay/name (sized (- n 1) x))))

#| Goal0 = State → Stream State |#

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

#| Goal0 |#
(define (succeed0 s) (stream s))
(define (fail0 s) (stream))

#| Goal0 → Goal0 → Goal0 |#
(define ((disj0 g1 g2) s) (mplus (g1 s) (g2 s)))
(define ((conj0 g1 g2) s) (bind (g1 s) g2))
