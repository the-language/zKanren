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
(provide succ pred fmap-1 fmap-n force-n pack lift2+)
(require "zk0.rkt")
(require "conf.rkt")

#| Size = Positive-Integer |#

#| Num → Num |#
(define (succ x) (+ 1 x))
(define (pred x) (- x 1))

#| Promise a → (a → b) → Promise b |#
(define-syntax-rule (fmap-1 x f) (delay/name (f (force x))))

#| Size → [Promise+ a] → (Pos → [a] → [Promise+ a] → b) → b |#
(define (fmap-n n xs f)
  (let loop ([n n] [xs xs] [ys '()] [zs '()])
    (cond
      [(zero? n) (let-values ([(xs1 zs1) (partition promise? (append ys xs))])
                   (f 0 (append zs1 zs) xs1))]
      [(null? xs) (if (null? ys)
                      (f n zs '())
                      (loop n ys '() zs))]
      [(promise? (car xs)) (fmap-1 (car xs) (λ (a) (loop (pred n) (cdr xs) (cons a ys) zs)))]
      [else (loop n (cdr xs) ys (cons (car xs) zs))])))

#| Size → Promise+ a → Promise+ a |#
(define (force-n n x)
  (cond
    [(zero? n) x]
    [(promise? n) (force-n (pred n) x)]
    [else x]))

#| Promise+ a → Promise+ a |#
(define-syntax-rule (pack x) (delay/name (force x)))

#| Goal2 = Goal1 |#

#| ([Goal1] → Goal1) → (Size → [Promise Goal2] → Goal2) |#
(define ((lift2+ f) local-max-size gs)
  (pack
   (force-n (length gs)
            (fmap-n local-max-size gs (λ (n g0s g1s) (f (append g0s g1s)))))))
