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
(struct thunk-0 (v))
(define (fmap-thunk0 f x)
  (thunk-0 (f (thunk-0-v x))))
(define-syntax-rule (freeze x)
  (lambda () (thunk-0 x)))
(define-syntax-rule (freeze-succ x)
  (lambda () x))
(define (thaw x)
  (if (procedure? x)
      (thaw (x))
      (thunk-0-v x)))
(define (thaw1/fmap f x)
  (if (procedure? x)
      (freeze-succ (f (x)))
      (fmap-thunk0 f x)))
(define (thunk? x)
  (or (procedure? x) (thunk-0? x)))
(define (thaw*/fmap f x)
  (cond
    ((procedure? x) (freeze-succ (thaw*/fmap f (x))))
    ((thunk-0? x) (fmap-thunk0 f x))
    (else (f x))))

(struct var (v))
