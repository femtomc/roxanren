#lang info
(define collection "roxanren")
(define deps '("base" "rosette"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/roxanren.scrbl" ())))
(define pkg-desc "An experimental fusion of miniKanren + virtualized access to SMT solvers (Rosette).")
(define version "0.0.1")
(define pkg-authors '(femtomc))
