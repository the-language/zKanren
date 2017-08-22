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
(provide (struct-out constraint) (struct-out state))

#| (State → Bool) → ID → Any → [Var] → Constraint |#
(struct constraint (check kind parm vars))

#| Vector Goal → Hash ID (Vector Constraint) → State |#
(struct state (g c))

#| Vector (Values (Vector Goal Vector) Constraint) → StatePatch |#
(struct state-patch (g c))

#| Hash a b → (a → b → Maybe c) → Hash a c |#
(define (hash-map+filter h f)
  (let ([r (hash)])
    (let loop ([iter (hash-iterate-first h)])
      (cond
        [(not iter) r]
        [(let-values ([(k v) (hash-iterate-key+value h iter)])
           (f k v)) => (λ (x) (hash-set! r k x) (loop (hash-iterate-next h iter)))]
        [else (loop (hash-iterate-next h iter))]))))

#| (Pos → a → Bool) → Vector a → Vector a |#
(define (vector-filter+ f v)
  (let ([l (vector-length v)])
  (let loop ([i 0] [r '()])
    (cond
      [(<= l i) (list->vector r)]
      [(let ([x (vector-ref v i)])
         (f i x)) (loop (+ i 1) (cons x r))]
      [else (loop (+ i 1) r)]))))

#| Vector a → Pos → Vector a |#
(define (vector-drop-at v i) (vector-append (vector-take i v) (vector-drop (+ i 1) v)))

#| State → State |#
(define (clean-constraints s)
  (let ([s-c (state-c s)])
  (state (state-g s)
         (hash-map+filter s-c
                          (λ (id constraints)
                            (vector-filter+ (λ (i c) ((constraint-delete c)

#| State → Maybe State |#
(define (run-constraints s) (error))
