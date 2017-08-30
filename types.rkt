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
 state?
 state-g
 state-c
 state-hg
 (rename-out [mkstate state])
 )
(require "goal.rkt")

#| [Goal] → Hash ID ConstraintsV → WeakSet Goal → State |#
(struct state (g c hg))

(define/contract (mkstate g c hg)
  ((listof goal?) hash? set-weak? . -> . state?)
  (state g c hg))
