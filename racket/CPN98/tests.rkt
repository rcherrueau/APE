#lang racket/base

(require rackunit
         rackunit/text-ui
         (submod "desugar.rkt" test)
         (submod "meta.rkt" test)
         (submod "simply-typed.rkt" test))

(run-tests desugar-tests)
(run-tests meta-tests)
(run-tests simply-typed-tests)
