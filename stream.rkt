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
 pack
 sizedstream
 sizedstream-cons
 promise+-fmap-flip
 sizedstream-mplus
 sizedstream-join
 sizedstream-bind
 sizedstream-map
 sizedstream-filter
 sizedstream->list
 sizedstream-take
 )

#| Promise+ a = U a (Promise (Promise+ a)) |#

#| U (Promise a) a → Promise a |#
(define-syntax-rule (pack x) (delay (force x)))

#| Promise+ a → (a → b) → Promise+ b |#
(define (promise+-fmap-flip x f)
  (if (promise? x)
      (delay (promise+-fmap-flip (force x) f))
      (f x)))

#| SizedStream a = Promise+ (U () (a × SizedStream a))|#

(define-syntax sizedstream
  (syntax-rules ()
    [(_) '()]
    [(_ e) (sizedstream-cons e '())]
    [(_ e0 e ...) (sizedstream-cons e0 (sizedstream e ...))]))

#| a → SizedStream a → SizedStream a |#
(define-syntax-rule (sizedstream-cons x xs) (cons x (delay xs)))

#| SizedStream a → U () (a × SizedStream a) |#
(define (pull x) (if (promise? x) (pull (force x)) x))

#| SizedStream a → Bool |#
(define (sizedstream-empty? x) (null? (pull x)))

#| SizedStream a → a |#
(define (sizedstream-car x) (car (pull x)))

#| SizedStream a → SizedStream a |#
(define (sizedstream-cdr x) (cdr (pull x)))

#| SizedStream a → SizedStream a → SizedStream a |#
(define (sizedstream-mplus xs ys)
  (cond
    ((null? xs) ys)
    ((promise? xs) (delay (sizedstream-mplus ys (force xs))))
    (else (cons (car xs) (sizedstream-mplus ys (cdr xs))))))

#| SizedStream (SizedStream a) → SizedStream a |#
(define (sizedstream-join xss)
  (promise+-fmap-flip xss
                      (λ (xss)
                        (if (null? xss)
                            '()
                            (sizedstream-mplus (car xss) (pack (sizedstream-join (cdr xss))))))))

#| SizedStream a → (a → SizedStream b) → SizedStream b |#
(define (sizedstream-bind xs f) (sizedstream-join (sizedstream-map f xs)))

#| (a → b) → SizedStream a → SizedStream b |#
(define (sizedstream-map f xs)
  (promise+-fmap-flip xs
                      (λ (xs)
                        (if (null? xs)
                            '()
                            (cons (f (car xs)) (pack (sizedstream-map f (cdr xs))))))))

#| (a → Bool) → SizedStream a → SizedStream a |#
(define (sizedstream-filter f xs)
  (promise+-fmap-flip
   xs
   (λ (xs)
     (cond
       [(null? xs) '()]
       [(f (car xs)) (cons (car xs) (pack (sizedstream-filter f (cdr xs))))]
       [else (pack (sizedstream-filter f (cdr xs)))]))))

#| SizedStream a → [a] |#
(define (sizedstream->list xs)
  (let ([xs (pull xs)])
    (if (null? xs)
        '()
        (cons (car xs) (sizedstream->list (cdr xs))))))

#| Pos → SizedStream a → [a] |#
(define (sizedstream-take n xs)
  (if (zero? n)
      '()
      (let ([xs (pull xs)])
        (if (null? xs)
            '()
            (cons (car xs) (sizedstream-take (- n 1) (cdr xs)))))))
