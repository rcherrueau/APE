#lang racket/base

(require rackunit
         rackunit/text-ui
         "neonate.rkt"
         "adder.rkt"
         "adder+let.rkt"
         )

(module+ test
  (run-tests
   (test-suite
    "Compiler tests"

    neonate-tests
    adder-tests
    adder+let-tests
    )))
