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
 (struct-out constraints)
 (struct-out constraint)
 define-constraints
 )

#| ConstraintV = Any |#

#| ID →
(ConstraintV → State → Maybe State) →
(State → Bool) →
(State → Maybe State) →
(State → Symbol × [Any]) →
Constraints |#
(struct constraints (id add check clean show))

#| ID → ConstraintV → Constraint |#
(struct constraint (type v))

#| Hash ID Constraints |#
(define constraintss (make-hash))

#| Constraints → () |#
(define (define-constraints x) (hash-set! constraintss (constraints-id x) x))
