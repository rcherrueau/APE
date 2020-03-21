#lang racket/base

(require rackunit
         rackunit/text-ui
         (submod "desugar.rkt" test))

(run-tests desugar-tests)
