#lang racket

(define template "~s\n> ~s\n")

(define-syntax (print-test stx)
  (syntax-case stx ()
    [(_ x)
     #'(printf template x (quote x))]
    [(_ x ...)
     #'(begin
         (printf template x (quote x))
         ...)]))

(provide (all-defined-out))
