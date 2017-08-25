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
 (struct-out state)
 (struct-out state-patch)
 patch
 patch-
 patch--
 define-state-cleaner-
 define-state-cleaner
 clean-state
 patch+
 hash-andmap
 check-constraints
 )
(require "constraint.rkt")
(require "stream.rkt")
(require "hash.rkt")

#| [Goal] → Hash ID ConstraintsV → State |#
(struct state (g c))

#| [Values [Goal] [Constraint]] → StatePatch |#
(struct state-patch (v))

#| State → StatePatch → Stream State |#
(define (patch s p) (patch- s (state-patch-v p)))

#| State → [Values [Goal] [Constraint]] → SizedStream State |#
(define (patch- s ps)
  (if (null? ps)
      (sizedstream s)
      (sizedstream-cons (patch-- s (car ps)) (patch- s (cdr ps)))))

#| State → Values [Goal] [Constraint] → State |#
(define (patch-- s p)
  (let-values ([(gs cs) p])
    (let ([nc (hash-copy (state-c s))])
      (for ([c cs])
        (let* ([t (constraint-type c)]
               [constraints (get-constraints t)]
               [empty (constraints-empty constraints)])
        (hash-update!
         nc
         t
         (λ (x) (or ((constraints-add constraints) x) empty))
         empty)))
      (state (append gs (state-g s)) nc))))


#| [State → Maybe State] |#
(define cleanc
  (list
   (λ (s)
     (let loop ([b #f] [s s] [fs (hash-map (state-c s)
                                           (λ (id c) (constraints-clean (get-constraints id))))])
       (cond
         [(null? fs) (and b s)]
         [((car fs) s) => (λ (s) (loop #t s (cdr fs)))]
         [else (loop b s fs)])))))

(define-syntax-rule (define-state-cleaner state body)
  (define-state-cleaner- (λ (state) body)))
#| State → Maybe State → () |#
(define (define-state-cleaner- f) (set! cleanc (cons f cleanc)))

#| State → State |#
(define (clean-state s)
  (let loop ([cs cleanc] [s s])
    (if (null? cs)
        s
        (let ([ns ((car cleanc) s)])
          (if ns
              (loop cleanc ns)
              (loop (cdr cleanc) s))))))

#| State → [StatePatch] → SizedStream State |#
(define (patch+ s p)
  (if (null? p)
      (sizedstream s)
      (sizedstream-bind (patch s (car p)) (λ (ns) (patch+ ns (cdr p))))))

#| State → Bool |#
(define (check-constraints s)
  (hash-andmap
   (λ (id cs) (constraints-check cs))
   (state-c s)))
