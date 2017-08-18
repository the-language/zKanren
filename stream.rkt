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
(provide (all-defined-out))

#| Stream a = U () (Promise+ (a ⨯ Stream a)) |#
#| a → Stream a → Stream a |#
(define (stream+-cons a d) (delay/name (cons a d)))

#| Stream a → a ⨯ Stream a |#
(define (pull xs) (if (promise? xs) (pull (force xs)) xs))

#| Positive-Integer → Stream a → [a] |#
(define (take-stream+ n xs)
  (if (zero? n)
      '()
      (let ([xs (pull xs)])
        (if (null? xs)
            '()
            (cons (car xs) (take (- n 1) (cdr xs)))))))

#| Stream a → [a] |#
(define (force-stream+ xs)
  (let ([xs (pull xs)])
    (if (null? xs)
        '()
        (cons (car xs) (force-stream+ (cdr xs))))))

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

#| [a] → Stream a |#
(define-syntax stream+
  (syntax-rules ()
    ((_) '())
    ((_ x xs ...) (stream+-cons x (stream+ xs ...)))))
