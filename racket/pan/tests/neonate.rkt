#lang racket/base

(require rackunit
         "../utils.rkt")

(provide neonate-tests)

(define neonate-tests
  (test-suite
   "neonate lang tests"

   (test-compile "neonate-42"
                  "../neonate.rkt"
                  "42"
                  (unlines "    .intel_syntax noprefix"
                           "    .global _the_asm_code"
                           "    .text"
                           ""
                           "_the_asm_code:"
                           "    mov eax, 42"
                           "    ret"))

   (test-not-compile "neonate-illtyped"
                      "../neonate.rkt"
                      "#t"
                      #rx"#%datum: expected exact-nonnegative-integer\n  at: #t")

   (test-not-compile "neonate-nonnegative"
                      "../neonate.rkt"
                      "-1"
                      #rx"#%datum: expected exact-nonnegative-integer\n  at: -1")


   ))
