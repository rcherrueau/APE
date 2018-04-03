#lang typed/racket/base

(require "utils.rkt")

(provide (all-defined-out))

(define-datatype Primitive1
  Add1
  Sub1)

(define-datatype Exp
  [Num   Number]
  [Prim1 Primitive1 Exp])
