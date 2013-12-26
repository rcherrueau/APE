;; Syntax: Meta-Programming Helpers
;; Introduction

#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (mylet stx)
  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))

  (define-syntax-class distinct-bindings
    #:description "sequence of disctinct binding pairs"
    (pattern (b:binding ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(b.var ...)))
                        "duplicate variable name"
             ;; with clauses are used to bind var and rhs as
             ;; attributes of `distinct-bindings'
             #:with (var ...) #'(b.var ...)
             #:with (rhs ...) #'(b.rhs ...)))

  (syntax-parse stx
    [(_ bs:distinct-bindings . body)
     #'((lambda (bs.var ...) . body) bs.rhs ...)]))
