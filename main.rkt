#lang racket/base

(module+ test (require rackunit))

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
