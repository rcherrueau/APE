#lang typed/racket/base

(require "define-datatype.rkt")

(provide (all-defined-out))

(define-datatype Primitive1
  Add1
  Sub1)

(define-datatype Exp
  [Num   Number]
  [Id    Symbol]
  [Prim1 Primitive1 Exp]
  [Let   (Listof (Pairof Symbol Exp)) Exp]
  )