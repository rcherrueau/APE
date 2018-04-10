#lang racket/base

(require rackunit
         rackunit/text-ui
         "../ast.rkt"
         "utils.rkt")

(provide neonate-tests)

(define-values (test-ast test-asm test-compile test-not-compile)
  (tests-for-lang "../neonate.rkt"))

(define neonate-tests
  (test-suite
   "neonate lang tests"

   (test-ast "neonate-42"
             "42"
             (Num 42))

   (test-compile "neonate-42"
                  "42"
                  (unlines "    .intel_syntax noprefix"
                           "    .global _the_asm_code"
                           "    .text"
                           ""
                           "_the_asm_code:"
                           "    mov eax, 42"
                           "    ret"))

   (test-not-compile "neonate-illtyped"
                      "#t"
                      #rx"#%datum: expected exact-nonnegative-integer\n  at: #t")

   (test-not-compile "neonate-nonnegative"
                      "-1"
                      #rx"#%datum: expected exact-nonnegative-integer\n  at: -1")
   ))

(module+ test (run-tests neonate-tests))
