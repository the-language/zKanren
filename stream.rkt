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
(provide stream-mplus stream-join stream-bind)

#| Stream a → Stream a → Stream a |#
(define-syntax-rule (stream-mplus xs ys) (stream-mplus- xs (delay/name ys)))
#| Stream a → Promise (Stream a) → Stream a |#
(define (stream-mplus- xs ys)
  (if (stream-empty? xs)
      (force ys)
      (stream-cons (stream-first xs) (stream-mplus (force ys) (stream-rest xs)))))

#| Stream (Stream a) → Stream a |#
(define (stream-join xss)
  (if (stream-empty? xss)
      empty-stream
      (stream-mplus (stream-first xss) (stream-join (stream-rest xss)))))

#| Stream a → (a → Stream b) → Stream b |#
(define (stream-bind xs f) (stream-join (stream-map f xs)))
