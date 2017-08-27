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
 (struct-out state-patch1)
 (struct-out state-patch)
 patch
 patch-
 patch--
 define-state-cleaner-
 define-state-cleaner
 clean-state
 patch+
 check-constraints
 get-constraintsv
 set-constraintsv
 new-state
 new-state+
 s+c
 s+c+
 )
(require "constraint.rkt")
(require "stream.rkt")
(require "hash.rkt")
(require "id.rkt")
(require "contract.rkt")

#| [Goal] → Hash ID ConstraintsV → State |#
(struct state (g c))

#| [Goal] → [Constraint] → StatePatch1 |#
(struct state-patch1 (gs cs))

#| [StatePatch1] → StatePatch |#
(struct state-patch (vs))

#| State → StatePatch → Stream State |#
(define (patch s p) (patch- s (state-patch-vs p)))

#| State → [StatePatch1] → SizedStream State |#
(define (patch- s ps)
  (cond
    [(null? ps) '()]
    [(patch-- s (car ps)) => (λ (ns) (sizedstream-cons ns (pack (patch- s (cdr ps)))))]
    [else (pack (patch- s (cdr ps)))]))

#| State → StatePatch1 → Maybe State |#
(define (patch-- s p)
  (let ([gs (state-patch1-gs p)] [cs (state-patch1-cs p)])
    (s+c+ cs (state (append gs (state-g s)) (state-c s)))))

#| [Constraint] → State → Maybe (State × [Var]) |#
(define/contract (s+c cs s)
  (-> (listof constraint?) state? (maybe (cons/c state? (listof var?))))
  (call/cc
   (λ (return)
     (let ([s s] [vs '()])
       (for ([c cs])
         (let* ([t (constraint-type c)]
                [constraints (get-constraints- t)]
                [nsv ((constraints-add constraints) (constraint-v c) s)])
           (if nsv
               (let ([ns (car nsv)] [nvs (cdr nsv)])
                 (set! s ns)
                 (set! vs (append nvs vs)))
               (return #f))))
       (return (cons s vs))))))

#| [Constraint] → State → Maybe State |#
(define/contract (s+c+ cs s)
  (-> (listof constraint?) state? (maybe state?))
  (let ([nsv (s+c cs s)])
    (and nsv
         (let ([s (car nsv)] [vs (cdr nsv)])
           (and (check-constraints vs s) s)))))

#| [State → Maybe State] |#
(define/contract cleanc
  (listof (-> state? (maybe state?)))
  (list
   (λ (s)
     (let loop ([b #f] [s s] [fs (hash-map (state-c s)
                                           (λ (id c) (constraints-clean (get-constraints- id))))])
       (cond
         [(null? fs) (and b s)]
         [((car fs) s) => (λ (s) (loop #t s (cdr fs)))]
         [else (loop b s fs)])))))

(define-syntax-rule (define-state-cleaner state body)
  (define-state-cleaner- (λ (state) body)))
#| (State → Maybe State) → () |#
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

#| [Var] → State → Bool |#
(define (check-constraints vs s)
  (hash-andmap
   (λ (id cs) ((constraints-check (get-constraints- id)) vs s))
   (state-c s)))

#| State → Constraints → ConstraintsV |#
(define (get-constraintsv s cs) (hash-ref (state-c s) (constraints-id cs) (constraints-empty cs)))

#| State → Constraints → ConstraintsV → State |#
(define (set-constraintsv s cs v) (hash-set (state-c s) (constraints-id cs) v))

#| Goal ... → State |#
(define (new-state . gs) (state gs (hash)))

#| U Goal Constraint ... → State |#
(define (new-state+ . gs)
  (let-values ([(c g) (partition constraint? gs)])
               (s+c+ c (state g (hash)))))
