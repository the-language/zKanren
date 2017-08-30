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
 new-goal
 run-goal
 (struct-out goal+)
 noto
 goal?
 )

#| Goal = U AGoal DGoal |#

#| StatePatch → Goal |#
(define-syntax-rule (new-goal x) (goal (delay x)))
#| Promise StatePatch → Goal |#
(struct goal (v))

#| Goal → StatePatch |#
(define (run-goal x)
  (force (goal-v x)))

#| U Constraint Goal → U Constraint Goal → Goal+ |#
(struct goal+ (s u))

#| Goal+ → Goal+ |#
(define (noto g) (goal+ (goal+-s g) (goal+-u g)))
