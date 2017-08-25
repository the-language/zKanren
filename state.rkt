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
 patch/check
 patch/check-
 patch/check--
 define-state-cleaner-
 define-state-cleaner
 clean-state
 patch/check+
 check-constraints
 get-constraintsv
 empty-state
 set-constraintsv
 new-state
 )
(require "constraint.rkt")
(require "stream.rkt")
(require "hash.rkt")

#| [Goal] → Hash ID ConstraintsV → State |#
(struct state (g c))

#| [Values [Goal] [Constraint]] → StatePatch |#
(struct state-patch (v))

#| State → StatePatch → Stream State |#
(define (patch/check s p) (patch/check- s (state-patch-v p)))

#| State → [Values [Goal] [Constraint]] → SizedStream State |#
(define (patch/check- s ps)
  (cond
    [(null? ps) '()]
    [(patch/check-- s (car ps)) => (λ (ns) (sizedstream-cons ns (pack (patch/check- s (cdr ps)))))]
    [else (pack (patch/check- s (cdr ps)))]))

#| State → Values [Goal] [Constraint] → Maybe State |#
(define (patch/check-- raws p)
  (call/cc
   (λ (return)
     (let-values ([(gs cs) p])
       (let ([vs '()] [s raws])
         (for ([c cs])
           (let* ([t (constraint-type c)]
                  [constraints (get-constraints- t)]
                  [nsv ((constraints-addv constraints) c s)])
             (if nsv
                 (let-values ([(ns nvs) nsv])
                   (set! s ns)
                   (set! vs (append nvs vs)))
                 (return #f))))
         (let ([s (state (append gs (state-g s)) (state-c s))])
           (return (and (check-constraints vs s) s))))))))

#| [State → Maybe State] |#
(define cleanc
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
(define (patch/check+ s p)
  (if (null? p)
      (sizedstream s)
      (sizedstream-bind (patch/check s (car p)) (λ (ns) (patch/check+ ns (cdr p))))))

#| [Var] → State → Bool |#
(define (check-constraints vs s)
  (hash-andmap
   (λ (id cs) ((constraints-check (get-constraints- id)) vs s))
   (state-c s)))

#| State → Constraints → ConstraintsV |#
(define (get-constraintsv s cs) (hash-ref (state-c s) (constraints-id cs) (constraints-empty cs)))

#| State |#
(define empty-state (state '() (hash)))

#| State → Constraints → ConstraintsV → State |#
(define (set-constraintsv s cs v) (hash-set (state-c s) (constraints-id cs) v))

#| Goal ... → State |#
(define (new-state . gs) (state gs (hash)))
