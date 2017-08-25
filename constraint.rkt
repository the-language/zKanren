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
 new-constraints
 new-constraint
 define-constraints-
 define-constraints
 get-constraints-
 )
(require "id.rkt")

#| ConstraintV = Any |#
#| ConstraintsV = Any |#

#| ID →
ConstraintsV →
(ConstraintV → State → Maybe (Values State [Var])) →
([Var] → State → Bool) →
(State → Maybe State) →
(State → Symbol × [Any]) →
Constraints |#
(struct constraints (id empty addv check clean show))

#| ConstraintsV →
(ConstraintV → State → Maybe (Values State [Var])) →
([Var] → State → Bool) →
(State → Maybe State) →
(State → Symbol × [Any]) →
Constraints |#
(define (new-constraints empty addv check clean show) (constraints (new-id) empty addv check clean show))

#| ID → ConstraintV → Constraint |#
(struct constraint (type v))

#| Constraints → ConstraintV → Constraint |#
(define (new-constraint cs v) (constraint (constraints-id cs) v))

#| Hash ID Constraints |#
(define constraintss (make-hash))

#| Constraints → () |#
(define (define-constraints- x)
  (let ([id (constraints-id x)])
    (if (hash-has-key? constraintss id)
        (error 'define-constraints-)
        (hash-set! constraintss id x))))

#| ID → Constraints |#
(define (get-constraints- id) (hash-ref constraintss id))

(define-syntax-rule (define-constraints name empty addv check clean show)
  (begin
    (define name (new-constraints empty addv check clean show))
    (define-constraints- name)))
