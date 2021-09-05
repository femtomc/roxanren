#lang info

(define collection "microKanren")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/microKanren.scrbl" ())))
(define pkg-desc "A Racket implementation of microKanren.")
(define version "0.0.1")
(define pkg-authors '(femtomc))
