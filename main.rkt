#lang racket/base

(require "core.rkt")
(require "mini.rkt")
(require "extensions.rkt")

(provide 
  fresh
  ==
  run
  run*
  appendo
  proof?)

;;; Test suite.

(module+ test (require rackunit)
         (check-equal? (run* q (== 'pea q)) 'pea)
         )
