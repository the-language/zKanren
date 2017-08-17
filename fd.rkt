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
(require "zk.rkt")
(require "stream.rkt")
(require "prelude.rkt")
(provide (all-defined-out))

#| State → Var → Maybe [Any] |#
(define (get-d s v) (hash-ref (state-d s) v #f))

#| Var → [Any] → State → State |#
(define (ext-d v d s) (state (state-s s)
                             (hash-set (state-d s) v (append (or (get-d s v) '()) d))
                             (state-c s)
                             (state-v s)))

#| [Any] → Var → Goal3 |#
(define (domo d v)
  (goal3 (goal1 (λ (s) (check-constraints-stream (ext-d v d s))))
         (goal3-u (membero v d))))

#| [Var] → State → Maybe State |#
(define (check-fd vs s)
  (if (ormap
       (λ (v)
         (equal? #f (do bind-maybe+
                      [d (hash-ref (state-d s) v nothing)]
                      [w (hash-ref (state-s s) v nothing)]
                      (member (walk* w (state-s s)) (map (λ (x) (walk* x (state-s s))) d))))) vs)
      #f
      s))
