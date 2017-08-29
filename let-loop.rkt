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
(provide
 let-loop
 let-loop-hash)

(define-syntax-rule (let-loop countinue x rxs ([v vv] ...) onnull body)
  (let ([ixs rxs])
    (let loop ([xs ixs] [v vv] ...)
      (if (null? xs)
          onnull
          (let ([x (car xs)] [countinue (λ (v ...) (loop (cdr xs) v ...))])
            body)))))

(define-syntax-rule (let-loop-hash countinue k val rhash ([v x] ...) onnull body)
  (let ([hash rhash])
    (let loop ([iter (hash-iterate-first hash)] [v x] ...)
      (if (not iter)
          onnull
          (let-values ([(k val) (hash-iterate-key+value hash iter)])
            (let ([countinue (λ (v ...) (loop (hash-iterate-next hash iter) v ...))])
            body))))))
