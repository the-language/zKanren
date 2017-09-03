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

(define-syntax-rule (define-simple-constraints- id f)
  (define-constraints id
    '()
    (λ (v s) (cons (set-constraintsv s id (cons v (get-constraintsv s id))) v))
    (λ (vs s) (error 'd))
    (λ (s) (cons (quote id) (get-constraintsv s id)))))

(define/contract (new-simple-constraint- cs xs)
  (constraints? (listof var?) . -> . constraint?)
  (new-constraint cs xs))
