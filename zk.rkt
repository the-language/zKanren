;;  zKanren: MicroKanren with Constraints and Optimizations
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
(define (id x) x)
(struct sized (s v))
(define max-size 500)
(define-syntax-rule (freeze1 x)
  (sized 1 (lambda () x)))
(define-syntax-rule (freeze-succ x)
  (sized (min (sized-s x) max-size) (sized-v x)))
(define (fmap-sized f x)
  (sized (sized-s x) (lambda () (f ((sized-v x))))))
(define (thaw* x) ((sized-v x)))
(define-syntax-rule (freeze/cons a d)
  (freeze1 (cons a d)))

(struct var (v))
