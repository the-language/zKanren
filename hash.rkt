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
(provide hash-map+filter-flip hash-andmap)

#| Hash a b → (a → b → Maybe c) → Hash a c |#
(define (hash-map+filter-flip h f)
  (let ([r (make-hash)])
    (let loop ([iter (hash-iterate-first h)])
      (cond
        [(not iter) r]
        [(let-values ([(k v) (hash-iterate-key+value h iter)])
           (f k v)) => (λ (x)
                         (hash-set! r (hash-iterate-key h iter) x)
                         (loop (hash-iterate-next h iter)))]
        [else (loop (hash-iterate-next h iter))]))))


#| (k → v → Bool) → Hash k v → Bool |#
(define (hash-andmap f h) (foldl (λ (x y) (and x y)) #t (hash-map h f)))
