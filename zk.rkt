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
#| a → a |#
(define (id x) x)
#| Nat → Var |#
(struct var (v))
#| Stream a |#
(define stream-nil (delay/name '()))
#| a → Stream a → Stream a |#
(define (stream-cons a d) (delay (cons a d)))
#| (State → Stream State) → Goal0 |#
(struct goal0 (v))
#| [Goal0] → ConjV |#
(struct conj-v (v))
#| [Goal0] → Goal1 → DisjV |#
(struct disj-v (h t))
(define disj-v-max 16)
#| Goal1 = U Goal0 ConjV DisjV |#

