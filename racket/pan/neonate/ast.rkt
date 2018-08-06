#lang typed/racket/base

(require "define-datatype.rkt")

(provide (all-defined-out))

(define-datatype Exp
  [Num Number])
