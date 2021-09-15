#lang racket/base

(require "../core.rkt")
(require "../mini.rkt")

(provide appendo)

(module+ test (require rackunit))

(define appendo (lambda (l s ls)
                  (conde
                    [(== `() l) (== s ls)]
                    [(fresh (a d res)
                       (== `(,a . ,d) l)
                       (== `(,a . ,res) ls)
                       (appendo d s res))])))

(module+ test 
  (check-equal? (run 1 (q) (appendo q `(d e) `(a b c d e)))
                `((a b c)))
  (check-equal? (run 1 (q) (appendo `(a b c) q `(a b c d e)))
                `((d e)))

  (check-equal? (run* (q) (fresh (x y) 
                            (== `(,x ,y) q) 
                            (appendo x y '(1 2 3 4 5))))
                '((() (1 2 3 4 5))
                  ((1) (2 3 4 5))
                  ((1 2) (3 4 5))
                  ((1 2 3) (4 5))
                  ((1 2 3 4) (5))
                  ((1 2 3 4 5) ())))
  )
