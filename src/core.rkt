#lang racket/base

;;;
;;; Code from:
;;; microKanren: A Minimal Function Core for Relational Programming.
;;; http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
;;; As well as: The Reasoned Schemer, Chapter 10.
;;;

(provide var 
         var?
         call/fresh
         walk
         disj
         conj
         ==
         unit
         unify)

(module+ test (require rackunit))

; ------------------------------------------- ;

;;;
;;; racket/base doesn't provide `assp`
;;; so here's a small recursive implementation.
;;;

(define (assp ch lst)
  (cond
    ((null? lst) #f)
    ((ch (car (car lst))) (car lst))
    ((null? (cdr lst)) #f)
    (else (assp ch (cdr lst)))))

(module+ test 
  (check-equal? (assp (lambda (v) (eq? v 5)) '((1 3) (5 5))) '(5 5))
  (check-equal? (assp (lambda (v) (eq? v 'pea)) '((5 4) ('pod 'pod))) #f)
  )

; ------------------------------------------ ;

;;;
;;; microKanren
;;;

(define var (lambda (x) (vector x)))
(define var? (lambda (x) (vector? x)))
(define (var=? x1 x2) (eq? (vector-ref x1 0)
                           (vector-ref x2 0)))

;; This version of `walk` just uses a list of pairs.
(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

;;;
;;; These tests come straight from The Reasoned Schemer, Chapter 10.
;;;

(module+ test
  (define v (var 'v))
  (define w (var 'w))
  (define x (var 'x))
  (define y (var 'y))
  (define z (var 'z))
  (check-equal? (walk y `((,z . a) (,z . ,w) (,y . ,z))) 'a)
  (check-equal? (walk w `((,x . ,y) (,v . ,x) (,w . ,x))) y)
  (check-equal? (walk w `((,x . 'b) (,z . ,y) (,w . (,x 'e ,z)))) 
                '(#(x) 'e #(z)))
  )

;; This is an alternative version of `walk` which uses a mutable hash map.

(define (hash-set-list! ht k v)
  (cond
    ((var? v) (hash-set! ht k v))
    (else 
      (let ((pr (hash-ref ht k #f)))
        (if pr (hash-set! ht k (append '(,v) pr)) 
          (hash-set! ht k '(,v)))
        ))))

(define (hash-walk u s)
  (let ((pr (and (var? u) (hash-ref s u #f))))
    (if pr (hash-walk pr s) u)))

(module+ test
  (define ht1 (make-hash))
  (hash-set! ht1 z 'a)
  (hash-set! ht1 z w)
  (hash-set! ht1 y z)
  (check-equal? (hash-walk y ht1) 'a)
  (define ht2 (make-hash))
  (hash-set! ht2 x y)
  (hash-set! ht2 v x)
  (hash-set! ht2 w x)
  (check-equal? (hash-walk w ht2) y)
  )

(define (ext-s x v s) `((,x . ,v) . ,s))

(define (unit s/c) (cons s/c mzero))
(define mzero `())

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))
