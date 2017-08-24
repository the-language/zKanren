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
 )
(require "constraint.rkt")

#| [Goal] → Hash ID ConstraintsV → State |#
(struct state (g c))

#| [Values [Goal] [Constraint]] → StatePatch |#
(struct state-patch (v))

#| State → StatePatch → Stream State |#
(define (patch s p) (patch- s (state-patch-v p)))

#| State → [Values [Goal] [Constraint]] → Stream State |#
(define (patch- s ps)
  (if (null? ps)
      (stream s)
      (stream-cons (patch-- s (car ps)) (patch- s (cdr ps)))))

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
     ((error 'c) (hash-map (state-c s)
                           (λ (id c) (constraints-clean (get-constraints id))))))))

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

#|
(provide
 (struct-out constraint)
 (struct-out state)
 (struct-out state-patch)
 define-state-cleaner
 define-state-cleaner-lambda
 clean-state
 hash-map+filter-flip
 check-constraints
 patch
 patch+
 )
(require "stream.rkt")

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
      (stream-bind (patch s (car p)) (λ (ns) (patch+ ns (cdr p))))))
|#
