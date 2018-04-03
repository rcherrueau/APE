#lang racket/base

(require rackunit
         rackunit/text-ui
         "neonate.rkt"
         )

(module+ test
  (run-tests
   (test-suite
    "Compiler tests"

    neonate-tests
    )))
