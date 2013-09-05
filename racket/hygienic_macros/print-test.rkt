#lang racket

(define-syntax (print-test stx)
  (syntax-case stx ()
    [(_ x)
     #'(printf "~s\n> ~s\n" x (quote x))]
    [(_ x ...)
     #'(begin
         (printf ">~s\n> ~s\n" x (quote x))
         ...)]))

(provide (all-defined-out))
