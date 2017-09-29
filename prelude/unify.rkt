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
 ==c
 =/=c
 ==
 =/=
 )
(require "../zk.rkt")
(require "../let-loop.rkt")
(require racket/struct)

#| ConstraintsV = Hash Var Any |#
#| ConstraintV = Any × Any |#

#| U Var a → Hash Var a → U Var a |#
(define (walk x h)
  (if (var? x)
      (let ([nv (hash-ref h x #f)])
        (if nv
            (walk nv h)
            x))
      x))

#| U Var a → [Var × Any] → U Var a |#
(define (walkl x l)
  (if (var? x)
      (let ([nv (assoc x l)])
        (if nv
            (walkl (cdr nv) l)
            x))
      x))

#| U Var a → Hash Var Any → [Var × Any] → U Var a |#
(define (walk2 x h l)
  (let ([nx (walkl (walk x h) l)])
    (if (equal? x nx)
        x
        (walk2 nx h l))))

#| ConstraintsV → Any → Any → Maybe [Var × Any] |#
(define (unify cv x y) (%unify cv '() x y))

#| ConstraintsV → [Var × Any] → Any → Any → Maybe [Var × Any] |#
(define (%unify cv l x y)
  (let ([x (walk2 x cv l)] [y (walk2 y cv l)])
    (cond
      [(equal? x y) '()]
      [(var? x) (list (cons x y))]
      [(var? y) (list (cons y x))]
      [(pair? x) (and (pair? y) (let ([xs (unify cv l (car x) (car y))] )
                                  (and xs (let ([ys (unify cv (append xs l) (cdr x) (cdr y))])
                                            (and ys (append xs ys))))))]
      [(vector? x) (and (vector? y) (unify cv (vector->list x) (vector->list y)))]
      [(struct? x) (and (struct? y) (unify cv (struct->vector x) (struct->vector y)))]
      [else #f])))

#| ConstraintsV → Any → Any → Bool |#
(define (unify? cv x y) (null? (unify cv x y)))
(define (ununify? cv x y) (not (unify cv x y)))

#| Any → Any → Constraint |#
(define (==- x y) (new-constraint ==c (cons x y)))
(define (=/=- x y) (new-constraint =/=c (cons x y)))

#| Any → Any → Goal+ |#
(define (== x y) (goal+ (==- x y) (=/=- x y)))
(define (=/= x y) (goal+ (=/=- x y) (==- x y)))

(define-constraints ==c
  (hash)
  (λ (cv s)
    (let ([x (car cv)] [y (cdr cv)] [csv (get-constraintsv s ==c)])
      (let ([nc (unify csv x y)])
        (and nc (let loop ([csv csv] [nc nc] [vs '()])
                  (if (null? nc)
                      (cons (set-constraintsv s ==c csv) vs)
                      (let ([a (car nc)])
                        (let ([v (car a)] [x (cdr a)])
                          (when (hash-has-key? csv v)
                            (error '==))
                          (loop (hash-set csv v x) (cdr nc) (cons v vs))))))))))
  (λ (vs s) #t)
  (λ (s) #f)
  (λ (s) (cons '== (hash->list (get-constraintsv s ==c)))))

#| ConstraintsV → Any → Any → Maybe [Var × Any] |#
(define (ununify cv x y)
  (let ([us (unify cv x y)])
    (cond
      [(not us) '()]
      [(null? us) #f]
      [else us])))

(require racket/set)
#| ConstraintsVUn = [NonEmptySet (Var × Any)] |#

#| Set (Var × Any) → ConstraintsVUn → ConstraintsVUn |#
(define (add=/= c cs)
  (let ([cs (filter-not (λ (c2) (subset? c c2)) cs)])
    (if (ormap (λ (c2) (subset? c2 c)) cs)
        cs
        (cons c cs))))

(struct %nothing ())
(define nothing (%nothing))

#| Var → ConstraintsV → ConstraintsVUn → U Bool ConstraintsVUn |#
(define (check=/=1 v csv csvu)
  (let ([x (hash-ref csv v nothing)])
    (or (%nothing? x) (check== v x csv csvu))))

#| (a → Bool) → Set a → Set a |#
(define (set-filter f s)
  (list->set (filter f (set->list s))))

#| (a → b) → Set a → Set b |#
(define (set-map f s)
  (list->set (map f (set->list s))))

#| Var → Any → ConstraintsV → ConstraintsVUn → Maybe ConstraintsVUn |#
(define (check== v x csv csvu)
  (let-loop loop s csvu [(ncsvu '())]
            ncsvu
            (let ([ns (set-filter (λ (y)
                                    (let ([v2 (car y)] [z (cdr y)])
                                      (if (unify? csv v v2)
                                          (not (unify? csv x z))
                                          #t))) s)])
              (if (set-empty? ns)
                  #f
                  (loop (cons ns ncsvu))))))

(define-constraints =/=c
  '()
  (λ (cv s)
    (let ([x (car cv)] [y (cdr cv)] [csv (get-constraintsv s ==c)] [csvu (get-constraintsv s =/=c)])
      (let ([nc (ununify csv x y)])
        (and nc (if (null? nc)
                    (cons s '())
                    (cons (set-constraintsv s =/=c (add=/= (list->set nc) csvu)) (map car nc)))))))
  (λ (vs s)
    (let ([csv (get-constraintsv s ==c)] [csvu (get-constraintsv s =/=c)])
      (let-loop loop v vs ([b #t] [ncsvu csvu])
                (or b (set-constraintsv s =/=c ncsvu))
                (let ([nncsvu (check=/=1 v csv csvu)])
                  (and nncsvu (if (pair? nncsvu)
                                  (loop #f nncsvu)
                                  (loop b ncsvu)))))))
  (λ (s)
    (let ([csv (get-constraintsv s ==c)] [csvu (get-constraintsv s =/=c)])
      (let ([r (clean-=/=-subset csv csvu)])
        (let ([ncsvu (clean-ununify csv (or r csvu))])
          (or ncsvu r)))))
  (λ (s) (cons '=/= (map set->list (get-constraintsv s =/=c)))))

#| ConstraintsV → ConstraintsVUn → U False ConstraintsVUn |#
(define (clean-ununify csv csvu)
  (let ([b #f])
    (and b (filter-not
            set-empty?
            (map
             (λ (s)
               (set-filter
                (λ (p)
                  (if (ununify? csv (car p) (cdr p))
                      (begin
                        (set! b #t)
                        #f)
                      #t))
                s))
             csvu)))))
(define (clean-=/=-subset csv csvu)
  (let-loop loop x csvu ([ncsvu '()])
            (if (< (length ncsvu) (length csvu))
                ncsvu
                #f)
            (loop (add=/= x ncsvu))))
