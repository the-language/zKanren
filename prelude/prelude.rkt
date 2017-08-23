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
(require "../zk.rkt")
(provide (all-defined-out))

(define (membero x xs)
  (conde
   ((caro xs x))
   ((fresh (d)
          (cdro xs d)
          (membero x d)))))

(define (caro p a)
  (fresh (d)
      (== (cons a d) p)))

(define (cdro p d)
  (fresh (a)
      (== (cons a d) p)))

(define (conso a d p)
  (== (cons a d) p))

(define (nullo x)
  (== x '()))
