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
(provide bind-maybe do nothing bind-maybe+ maybe+->maybe maybe->maybe+)

#| Maybe a → (a → Maybe b) → Maybe b |#
(define (bind-maybe x f) (if x (f x) #f))

(define-syntax do
  (syntax-rules (<-)
    ((_ bind x) x)
    ((_ bind x <- mx xs ...) (bind mx (λ (x) (do bind xs ...))))
    ((_ bind mx xs ...) (bind mx (λ (x) (do bind xs ...))))))

(struct nothing- ())
#| Maybe+ |#
(define nothing (nothing-))

#| Maybe+ a → (a → Maybe+ b) → Maybe+ b |#
(define (bind-maybe+ x f) (if (equal? x nothing) nothing (f x)))

#| Maybe+ a → Maybe a |#
(define (maybe+->maybe x) (if (equal? x nothing) #f x))

#| Maybe a → Maybe+ a |#
(define (maybe->maybe+ x) (if x x nothing))
