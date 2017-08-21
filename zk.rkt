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
(provide (struct-out state) empty-state walk)
(require racket/struct)

#| (s : Hash Var Any) → (d : Hash Var [Any]) → (c : [Constraint]) → (v : Nat) → State |#
(struct state (s d c v))

#| State |#
(define empty-state (state (make-immutable-hash) (make-immutable-hash) '() 0))

#| a → State → a |#
(define (walk x s) (hash-ref (state-s x) x x))

#|
#| Struct → Struct |#
(define (struct-map f x)
  (apply (struct-type-make-constructor
          (let-values ([(r b) (struct-info x)]) r))
         (map (struct->list x) f)))

#| a → State → a |#
(define (walk* x s)
  (let ([x (walk x s)])
    (cond
      [(pair? x) (cons (walk* (car x) s) (walk* (cdr x) s))]
      [(vector? x) (vector-map (λ (x) (walk* x s)) x)]
      [(struct? x) (struct-map (λ (x) (walk* x s)) x)]
      [else x])))
|#
