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
 promise+/c
 sizedstream/c
 sizedstream?
 )

#| Promise+ a = U a (Promise (Promise+ a)) |#
#| Contract → Contract |#
(define (promise+/c t)
  (or/c
   t
   (promise/c (recursive-contract (promise+/c t)))))

#| U (Promise a) a → Promise a |#
(define-syntax-rule (pack x) (delay (force x)))

#| Promise+ a → (a → b) → Promise+ b |#
(define/contract (promise+-fmap-flip x f)
  (let ([a (new-∀/c 'a)] [b (new-∀/c 'b)])
    (-> (promise+/c a) (-> a b) (promise+/c b)))
  (if (promise? x)
      (delay (promise+-fmap-flip (force x) f))
      (f x)))

#| SizedStream a = Promise+ (U () (a × SizedStream a))|#
#| Contract → Contract |#
(define (sizedstream/c t)
  (or/c
   null?
   (cons/c t (recursive-contract (sizedstream/c t)))
   (promise/c (recursive-contract (sizedstream/c t)))))
#| Contract |#
(define sizedstream? (sizedstream/c any/c))

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
(define/contract (sizedstream-car x)
  (let ([a (new-∀/c 'a)])
    ((sizedstream/c a) . -> . a))
  (car (pull x)))

#| SizedStream a → SizedStream a |#
(define/contract (sizedstream-cdr x)
  (let ([a (new-∀/c 'a)])
    ((sizedstream/c a) . -> . (sizedstream/c a)))
  (cdr (pull x)))

#| SizedStream a → SizedStream a → SizedStream a |#
(define/contract (sizedstream-mplus xs ys)
  (let ([a (new-∀/c 'a)])
    ((sizedstream/c a) (sizedstream/c a) . -> . (sizedstream/c a)))
  (cond
    ((null? xs) ys)
    ((promise? xs) (delay (sizedstream-mplus ys (force xs))))
    (else (cons (car xs) (sizedstream-mplus ys (cdr xs))))))

#| SizedStream (SizedStream a) → SizedStream a |#
(define/contract (sizedstream-join xss)
  (let ([a (new-∀/c 'a)])
    ((sizedstream/c (sizedstream/c a)) . -> . (sizedstream/c a)))
  (promise+-fmap-flip xss
                      (λ (xss)
                        (if (null? xss)
                            '()
                            (sizedstream-mplus (car xss) (pack (sizedstream-join (cdr xss))))))))

#| SizedStream a → (a → SizedStream b) → SizedStream b |#
(define/contract (sizedstream-bind xs f)
  (let ([a (new-∀/c 'a)] [b (new-∀/c 'b)])
    ((sizedstream/c a) (a . -> . (sizedstream/c b)) . -> . (sizedstream/c b)))
  (sizedstream-join (sizedstream-map f xs)))

#| (a → b) → SizedStream a → SizedStream b |#
(define/contract (sizedstream-map f xs)
  (let ([a (new-∀/c 'a)] [b (new-∀/c 'b)])
    ((a . -> . b) (sizedstream/c a) . -> . (sizedstream/c b)))
  (promise+-fmap-flip xs
                      (λ (xs)
                        (if (null? xs)
                            '()
                            (cons (f (car xs)) (pack (sizedstream-map f (cdr xs))))))))

#| (a → Bool) → SizedStream a → SizedStream a |#
(define/contract (sizedstream-filter f xs)
  (let ([a (new-∀/c 'a)])
    ((a . -> . boolean?) (sizedstream/c a) . -> . (sizedstream/c a)))
  (promise+-fmap-flip
   xs
   (λ (xs)
     (cond
       [(null? xs) '()]
       [(f (car xs)) (cons (car xs) (pack (sizedstream-filter f (cdr xs))))]
       [else (pack (sizedstream-filter f (cdr xs)))]))))

#| SizedStream a → [a] |#
(define/contract (sizedstream->list xs)
  (let ([a (new-∀/c 'a)])
    ((sizedstream/c a) . -> . (listof a)))
  (let ([xs (pull xs)])
    (if (null? xs)
        '()
        (cons (car xs) (sizedstream->list (cdr xs))))))

#| Pos → SizedStream a → [a] |#
(define/contract (sizedstream-take n xs)
  (let ([a (new-∀/c 'a)])
    (exact-nonnegative-integer? (sizedstream/c a) . -> . (listof a)))
  (if (zero? n)
      '()
      (let ([xs (pull xs)])
        (if (null? xs)
            '()
            (cons (car xs) (sizedstream-take (- n 1) (cdr xs)))))))
