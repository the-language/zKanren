;;  zKanren: MicroKanren with Constraints and Optimizations
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
(define (id x) x)
(struct sized (s v))
(define max-size 500)
(define-syntax-rule (freeze1 x)
  (sized 1 (lambda () x)))
(define (freeze-succ x)
  (sized (min (+ 1 (sized-s x)) max-size) (sized-v x)))
(define (fmap-sized f x)
  (sized (sized-s x) (lambda () (f ((sized-v x))))))
(define (thaw* x) ((sized-v x)))
(define-syntax-rule (freeze/cons a d)
  (freeze1 (cons a d)))
(define (fmap-sized1 f xs)
  (let ((a (car xs)) (d (cdr xs)))
    (let loop ((x-s (sized-s a)) (x-v (sized-v a)) (xs d) (ys '()))
      (if (null? xs)
          (sized x-s (lambda () (f (x-v) ys)))
          (let ((y (car xs)) (xs (cdr xs)))
            (if (< (sized-s y) x-s)
                (loop (sized-s y) (sized-v y) xs (cons (sized x-s x-v) ys))
                (loop x-s x-v xs (cons y ys))))))))
(struct disj-v (v))
(struct conj-v (v))
(struct goal (s u))
(define (mplus xs ys)
  (cond
    ((null? xs) ys)
    ((> (sized-s xs) (sized-s ys)) (mplus (ys xs)))
    (else (fmap-sized (lambda (xs)
                        (cons (car xs) (mplus (cdr xs) ys)))
                      xs))))
(define (bind xs f)
  (if (null? xs)
      '()
      (fmap-sized (lambda (xs)
                    (mplus (f (car xs)) (bind (cdr xs) f)))
                  xs)))
(define (disj fs)
  (fmap-sized1 (lambda (f fs)
                 (let loop ((f f) (fs fs))
                   (if (null? fs)
                       f
                       (lambda (s)
                         (mplus (f s) (fmap-sized1 (lambda (f fs) ((loop f fs) s)) fs))))))
               fs))
(define (fmap-sized* f fs)
  (if (null? fs)
      (f '())
      (fmap-sized (lambda (h) (fmap-sized* (lambda (t) (f (cons h t))) (cdr fs))) (car fs))))
(define (conj fs)
  (fmap-sized* (lambda (fs)
                 (let loop ((f (car fs)) (fs (cdr fs)))
                   (if (null? fs)
                       f
                       (lambda (s)
                         (bind (f s) (loop (car fs) (cdr fs)))))))
               fs))
(define (disj+ fs)
  (disj (map freeze-succ fs)))
(define (conj+ fs)
  (conj (map freeze-succ fs)))
(define (disj++ fs)
  (let loop ((f (car fs)) (fs (cdr fs)) (ys '()))
    (cond
      ((not f) (if (null? fs)
                   (disj-v ys)
                   (loop (car fs) (cdr fs) ys)))
      ((disj-v? f) (loop #f fs (append (disj-v-v f) ys)))
      ((conj-v? f) (loop #f fs (cons (conj+ (conj-v-v f)) ys)))
      (else (loop #f fs (cons f ys))))))
(define (conj++ fs)
  (let loop ((f (car fs)) (fs (cdr fs)) (ys '()))
    (cond
      ((not f) (if (null? fs)
                   (conj-v ys)
                   (loop (car fs) (cdr fs) ys)))
      ((conj-v? f) (loop #f fs (append (conj-v-v f) ys)))
      ((disj-v? f) (loop #f fs (cons (disj+ (disj-v-v f)) ys)))
      (else (loop #f fs (cons f ys))))))

(struct var (v))
