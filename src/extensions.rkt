#lang racket/base

;;; User-level extensions.
;;;
;;; Many of these are taken from:
;;; A unified approach to solving seven programming pearls (functional pearl)
;;; https://dl.acm.org/doi/10.1145/3110252

(require "core.rkt")
(require "mini.rkt")

(provide appendo)

(define appendo (lambda (l s ls)
                  (conde
                    [(== `() l) (== s ls)]
                    [(fresh (a d res)
                            (== `(,a . ,d) l)
                            (== `(,a . ,res) ls)
                            (appendo d s res))])))
