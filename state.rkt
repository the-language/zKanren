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
(require "types.rkt")
(require "let-loop.rkt")

#| [Goal] → [Constraint] → StatePatch1 |#
(struct state-patch1 (gs cs))

#| [StatePatch1] → StatePatch |#
(struct state-patch (vs))

#| State → StatePatch → Stream State |#
(define/contract (patch s p)
  (-> state? state-patch? (sizedstream/c state?))
  (patch- s (state-patch-vs p)))

#| State → [StatePatch1] → SizedStream State |#
(define/contract (patch- s ps)
  (-> state? (listof state-patch1?) (sizedstream/c state?))
  (cond
    [(null? ps) '()]
    [(patch-- s (car ps)) => (λ (ns) (sizedstream-cons ns (pack (patch- s (cdr ps)))))]
    [else (pack (patch- s (cdr ps)))]))

#| State → StatePatch1 → Maybe State |#
(define/contract (patch-- s p)
  (-> state? state-patch1? (maybe state?))
  (let ([gs (state-patch1-gs p)] [cs (state-patch1-cs p)])
    (s+c+ cs (state (append gs (state-g s)) (state-c s) (state-hg s)))))

#| [Constraint] → State → Maybe (State × [Var]) |#
(define/contract (s+c cs s)
  (-> (listof constraint?) state? (maybe (cons/c state? (listof var?))))
  (let-loop loop c cs ([s s] [vs '()])
            (cons s vs)
            (let* ([t (constraint-type c)]
                   [constraints (get-constraints- t)]
                   [nsv ((constraints-add constraints) (constraint-v c) s)])
              (and nsv (loop (car nsv) (append vs (cdr nsv)))))))

(define/contract (s+c+ cs s)
  ((listof constraint?) state? . -> . (maybe state?))
  (let ([nsv (s+c cs s)])
    (and nsv
         (let ([s (car nsv)] [vs (cdr nsv)])
           (and (check-constraints vs s) s)))))

(define/contract cleanc
  (listof (state? . -> . (maybe state?)))
  (list
   (λ (s)
     (let loop ([b #f] [s s] [fs (hash-map (state-c s)
                                           (λ (id c) (constraints-clean (get-constraints- id))))])
       (cond
         [(null? fs) (and b s)]
         [((car fs) s) => (λ (s) (loop #t s (cdr fs)))]
         [else (loop b s (cdr fs))])))))

(define-syntax-rule (define-state-cleaner state body)
  (define-state-cleaner- (λ (state) body)))
(define/contract (define-state-cleaner- f)
  ((state? . -> . (maybe state?)) . -> . void?)
  (set! cleanc (cons f cleanc)))

(define/contract (clean-state s)
  (state? . -> . state?)
  (let-loop loop f cleanc ([s s])
            s
            (let ([ns (f s)])
              (if ns
                  (clean-state ns)
                  (loop s)))))

(define/contract (patch+ s p)
  (state? (listof state-patch?) . -> . (sizedstream/c state?))
  (if (null? p)
      (sizedstream s)
      (sizedstream-bind (patch s (car p)) (λ (ns) (patch+ ns (cdr p))))))

(define/contract (check-constraints vs s)
  ((listof var?) state? . -> . (or/c state? boolean?))
  (let-loop-hash loop id cs (state-c s) ([nm #t] [s s])
                 (or nm s)
                 (let ([ns ((constraints-checkm (get-constraints- id)) vs s)])
                   (and ns (if (boolean? ns)
                               (loop nm s)
                               (loop #f ns))))))

#| Goal ... → State |#
(define (new-state . gs) (state gs (hash)))

#| U Goal Constraint ... → State |#
(define (new-state+ . gs)
  (let-values ([(c g) (partition constraint? gs)])
               (s+c+ c (state g (hash) (weak-set)))))
