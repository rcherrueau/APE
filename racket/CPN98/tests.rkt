#lang racket/base

(require rackunit
         rackunit/text-ui
         (submod "desugar.rkt" test)
         (submod "meta.rkt" test)
         (submod "basic-check.rkt" test))

(run-tests desugar-tests)
(run-tests meta-tests)
(run-tests basic-check-tests)
