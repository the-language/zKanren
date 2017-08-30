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
(provide id? new-id (rename-out [var/c var?]) new-var)

#| Any → Bool |#
(define id? exact-positive-integer?)

#| Nat |#
(define id-count 0)
#| → Pos |#
(define/contract (new-id)
  (-> id?)
  (set! id-count (+ 1 id-count))
  id-count)

#| Any → Bool |#
(define (var/c v) (and (var? v) (exact-positive-integer? v)))

#| Pos → Var |#
(struct var (id)
  #:methods gen:custom-write
  [(define (write-proc var port mode)
     (when mode (write-string "<" port))
     (display "var:" port)
     (display (var-id var) port)
     (when mode (write-string ">" port)))])
#| Nat |#
(define var-count 0)
#| → Var |#
(define/contract (new-var)
  (-> var/c)
  (set! var-count (+ 1 var-count))
  (var var-count))
