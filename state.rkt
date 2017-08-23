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
 (struct-out constraint)
 (struct-out state)
 (struct-out state-patch)
 define-constraints-cleaner
 clean-constraints
 hash-map+filter-flip
 check-constraints
 patch
 patch+
 )

#| Hash a b → (a → b → Maybe c) → Hash a c |#
(define (hash-map+filter-flip h f)
  (let ([r (make-hash)])
    (let loop ([iter (hash-iterate-first h)])
      (cond
        [(not iter) r]
        [(let-values ([(k v) (hash-iterate-key+value h iter)])
           (f k v)) => (λ (x)
                         (hash-set! r (hash-iterate-key h iter) x)
                         (loop (hash-iterate-next h iter)))]
        [else (loop (hash-iterate-next h iter))]))))

#| (a → b → Bool) → Hash a b → Bool |#
(define (hash-andmap f h) (andmap (λ (x) (f (car x) (cdr x))) (hash->list h)))

#| (State → Bool) → ID → Any → Vector Var → Constraint |#
(struct constraint (check kind parm vars))

#| [Goal] → Hash ID [Constraint] → State |#
(struct state (g c))

#| [Values [Goal] [Constraint]] → StatePatch |#
(struct state-patch (v))

#| [State → Maybe State] |#
(define cleanc '())

(define-syntax-rule (define-constraints-cleaner state body)
  (set! cleanc (cons (λ (state) body) cleanc)))

#| State → State |#
(define (clean-constraints s)
  (let loop ([cs cleanc] [s s])
    (if (null? cs)
        s
        (let ([ns ((car cleanc) s)])
          (if ns
              (loop cleanc ns)
              (loop (cdr cleanc) s))))))

#| State → Bool |#
(define (check-constraints s)
  (hash-andmap
   (λ (id cs)
     (andmap
      (λ (c)
        ((constraint-check c) s))
      cs))
   (state-c s)))

#| State → StatePatch → Stream State |#
(define (patch s p) (patch- s (state-patch-v p)))

#| State → [Values [Goal] [Constraint]] → Stream State |#
(define (patch- s p)
  (if (null? p)
      (stream s)
      (stream-cons (patch-- s (car p)) (patch- s (cdr p)))))

#| State → Values [Goal] [Constraint] → State |#
(define (patch-- s p)
  (let-values ([(gs cs) p])
    (let ([nc (hash-copy (state-c s))])
      (for ([c cs])
        (hash-update! nc (constraint-kind c) (λ (xs) (cons c xs)) '()))
      (state (append gs (state-g s)) nc))))

#| State → [StatePatch] → Stream State |#
(define (patch+ s p)
  (if (null? p)
      (stream s)
      (stream-append (patch s (car p)) (patch+ s (cdr p)))))
