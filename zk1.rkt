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
(provide )
(require "zk0.rkt")

#| Promise+ a → (a → b) → Promise+ b |#
(define (fmap x f) (if (promise? x) (delay/name (fmap (force x) f)) (f x)))

#| Promise+ a = U a (Promise (Promise+ a)) |#
#| Goal1 = Promise+ Goal0 |#

#| Goal1 Goal0 |#
(define ((goal1->goal0 g) s) (fmap g ($ s)))

#| (Goal0 → Goal0 → Goal0) → ([Goal1] → Goal1) |#
(define ((lift1 f) gs)
  (let-values ([(g0s g1s) (partition promise? gs)])
    (let loop ([g0s g0s] [g1s g1s])
      (if (null? g0s)
          (if (null? g1s)
              succeed0
              (sized (length g1s) (let-values ([(g0s1 g1s1) (partition promise? (map force g1s))])
                                  (loop g0s1 g1s1))))
          (f (car g0s) (goal1->goal0 (loop (cdr g0s) g1s)))))))
