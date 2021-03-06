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
 (contract-out (struct constraint
                 [(type id?)
                  (v constraintv?)]))
 new-constraints
 new-constraint
 define-constraints-
 define-constraints
 get-constraints-
 constraintv?
 constraintsv?
 get-constraintsv
 set-constraintsv
 )
(require "id.rkt")
(require "types.rkt")
(require "contract.rkt")

#| ConstraintV = Any |#
(define constraintv? any/c)
#| ConstraintsV = Any |#
(define constraintsv? any/c)

#| ID →
ConstraintsV →
(ConstraintV → State → Maybe (State × [Var])) →
([Var] → State → U Bool State) →
(State → Maybe State) →
(State → Symbol × [Any]) →
Constraints |#
(struct constraints (id empty add checkm clean show))

#| ConstraintsV →
(ConstraintV → State → Maybe (State × [Var])) →
([Var] → State → U Bool State) →
(State → Maybe State) →
(State → Symbol × [Any]) →
Constraints |#
(define/contract (new-constraints empty add check clean show)
  (-> constraintsv?
      (-> constraintv? state? (maybe (cons/c state? (listof var?))))
      (-> (listof var?) state? (or/c state? boolean?))
      (-> state? (maybe state?))
      (-> state? (cons/c symbol? list?))
      constraints?)
  (constraints (new-id) empty add check clean show))

#| ID → ConstraintV → Constraint |#
(struct constraint (type v))

#| Constraints → ConstraintV → Constraint |#
(define/contract (new-constraint cs v)
  (-> constraints? constraintv? constraint?)
  (constraint (constraints-id cs) v))

#| Hash ID Constraints |#
(define constraintss (make-hash))

(define/contract (define-constraints- x)
  (-> constraints? void?)
  (let ([id (constraints-id x)])
    (if (hash-has-key? constraintss id)
        (error 'define-constraints-)
        (hash-set! constraintss id x))))

#| ID → Constraints |#
(define (get-constraints- id) (hash-ref constraintss id))

(define-syntax-rule (define-constraints name empty add check clean show)
  (begin
    (define name (new-constraints empty add check clean show))
    (define-constraints- name)))

(define/contract (get-constraintsv s cs)
  (state? constraints? . -> . constraintsv?)
  (hash-ref (state-c s) (constraints-id cs) (constraints-empty cs)))

(define/contract (set-constraintsv s cs v)
  (state? constraints? constraintsv? . -> . state?)
  (state (state-g s) (hash-set (state-c s) (constraints-id cs) v) (state-hg s)))
