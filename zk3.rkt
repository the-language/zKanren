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
(provide (all-defined-out))
(require "zk2.rkt")

#| Goal3 = (succeed : Promise Goal2) ⨯ (fail : Promise Goal2) |#

#| Goal2 → Goal2 → Goal3 |#
(define-syntax-rule (goal3 s u) (cons (delay/name s) (delay/name u)))

#| Goal3 → Goal2 |#
(define (goal3-s g) (pack (force (car g))))
(define (goal3-u g) (pack (force (cdr g))))

#| Promise Goal3 → Goal2 |#
(define (pgoal3-s g) (delay/name (goal3-s (force g))))
(define (pgoal3-u g) (delay/name (goal3-u (force g))))

#| (Size → [Promise Goal2] → Goal2) → (Size → [Promise Goal3] → Goal3) |#
(define ((lift3+ f) local-max-size gs) (goal3 (f local-max-size (map pgoal3-s gs)) (f local-max-size (map pgoal3-u gs))))
