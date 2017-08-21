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
(provide sized mplus+ disj0 conj0)
(require "stream.rkt")

#| Nat → a → Promise+ a |#
(define-syntax-rule (sized n x) (raw-sized n (delay/name x)))
#| Nat → Promise a → Promise+ a |#
(define (raw-sized n x) (if (zero? n) (force x) (delay/name (sized (- n 1) x))))

#| Goal0 = State → Stream State |#

#| [Stream a] → Stream a |#
(define (mplus+ xs)
  (let-values ([(p s) (partition promise? xs)])
    (let loop ([p p] [s s])
      (if (null? p)
          (if (null? s)
              '()
              (sized (length s) (let-values ([(p1 s1) (partition promise? (map force s))])
                                  (loop p1 s1))))
          (let ([pa (car p)])
            (cons (car pa) (if (promise? (cdr pa))
                               (loop (cdr p) (cons (cdr pa) s))
                               (loop (cons (cdr pa) p) s))))))))


#| Goal0 → Goal0 → Goal0 |#
(define ((disj0 g1 g2) s) (mplus (g1 s) (g2 s)))
(define ((conj0 g1 g2) s) (bind (g1 s) g2))
