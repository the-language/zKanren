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
 (struct-out agoal)
 (struct-out dgoal)
 new-agoal
 new-dgoal
 run-goal
 (struct-out goal+)
 noto
 )

#| Goal = U AGoal DGoal |#

#| StatePatch → AGoal |#
(define-syntax-rule (new-agoal x) (agoal (delay/name x)))
#| Promise StatePatch → AGoal |#
(struct agoal (v));anonymous-goal

#| ID → [Any] → StatePatch → DGoal |#
(define-syntax-rule (new-dgoal id parm x) (dgoal id parm x))
#| ID → [Any] → Promise StatePatch → DGoal |#
(struct dgoal (id parm v));define-goal

#| Goal → StatePatch |#
(define (run-goal x)
  (if (agoal? x)
      (force (agoal-v x))
      (force (dgoal-v x))))

#| U Constraint Goal → U Constraint Goal → Goal+ |#
(struct goal+ (s u))

#| Goal+ → Goal+ |#
(define (noto g) (goal+ (goal+-s g) (goal+-u g)))
