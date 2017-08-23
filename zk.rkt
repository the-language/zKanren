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

(require "state.rkt")
(require "goal.rkt")

(define-state-cleaner s
  (state (remove-duplicates (state-g s)) (state-c s)))

(define-state-cleaner s
  (state (state-g s) (hash-map+filter-flip (state-c s)
                                           (λ (id cs)
                                             (let ([ncs (remove-duplicates cs)])
                                               (if (null? ncs)
                                                   #f
                                                   ncs))))))

#| Stream State → Stream State |#
(define (check-states ss) (stream-filter check-constraints ss))

#| State → Stream State |#
(define (pass s)
  (let ([g (state-g s)] [c (state-c s)])
    (if (null? g)
        s
        (stream-filter check-constraints (stream-map clean-state (patch+ (state '() c) (map run-goal g)))))))
