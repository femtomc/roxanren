#lang racket/base

;;; User-level extensions.
;;;
;;; Many of these are taken from:
;;; A unified approach to solving seven programming pearls (functional pearl)
;;; https://dl.acm.org/doi/10.1145/3110252

(require "mini.rkt")

(provide appendo
         proof?)

(define appendo (lambda (l s ls)
                  (conde
                    [(== `() l) (== s ls)]
                    [(fresh (a d res)
                            (== `(,a . ,d) l)
                            (== `(,a . ,res) ls)
                            (appendo d s res))])))

(define proof?
  (lambda (proof)
    (match proof
           [`(,A ,assms assumption ()) (member? A assms)]
           [`(,B ,assms modus-ponens (((,A => ,B) ,assms ,r1 ,ants1)
                                      (,A ,assms ,r2 ,ants2)))
             (and (proof? `((,A => ,B) ,assms ,r1 ,ants1))
                  (proof? `(,A ,assms ,r2 ,ants2)))]
           [`((,A => ,B) ,assms conditional ((,B (,A . ,assms) ,rule ,ants)))
             (proof? `(,B (,A . ,assms) ,rule ,ants))])))
