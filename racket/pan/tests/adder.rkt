#lang racket/base

(require rackunit
         rackunit/text-ui
         "../ast.rkt"
         "utils.rkt")

(provide adder-tests)

(define adder-tests
  (test-suite
   "adder lang tests"


   ;; Tests AST
   (test-ast "adder-42"
             "../adder.rkt"
             "42"
             (Num 42))

   (test-ast "adder-add1-42"
                 "../adder.rkt"
                 "(add1 42)"
                 (Prim1 (Add1) (Num 42)))

   (test-ast "adder-sub1-42"
                 "../adder.rkt"
                 "(sub1 42)"
                 (Prim1 (Sub1) (Num 42)))

   (test-ast "adder-sub1-add1-add1-42"
                 "../adder.rkt"
                 "(sub1 (add1 (add1 42)))"
                 (Prim1 (Sub1) (Prim1 (Add1) (Prim1 (Add1) (Num 42)))))


   ;; Tests compile
   (test-compile "adder-42"
                  "../adder.rkt"
                  "42"
                  (unlines "    .intel_syntax noprefix"
                           "    .global _the_asm_code"
                           "    .text"
                           ""
                           "_the_asm_code:"
                           "    mov eax, 42"
                           "    ret"))

   (test-compile "adder-add1-42"
                 "../adder.rkt"
                 "(add1 42)"
                 (unlines "    .intel_syntax noprefix"
                          "    .global _the_asm_code"
                          "    .text"
                          ""
                          "_the_asm_code:"
                          "    mov eax, 42"
                          "    add eax, 1"
                          "    ret"))

   (test-compile "adder-sub1-42"
                 "../adder.rkt"
                 "(sub1 42)"
                 (unlines "    .intel_syntax noprefix"
                          "    .global _the_asm_code"
                          "    .text"
                          ""
                          "_the_asm_code:"
                          "    mov eax, 42"
                          "    sub eax, 1"
                          "    ret"))

   (test-compile "adder-sub1-add1-add1-42"
                 "../adder.rkt"
                 "(sub1 (add1 (add1 42)))"
                 (unlines "    .intel_syntax noprefix"
                          "    .global _the_asm_code"
                          "    .text"
                          ""
                          "_the_asm_code:"
                          "    mov eax, 42"
                          "    add eax, 1"
                          "    add eax, 1"
                          "    sub eax, 1"
                          "    ret"))


   ;; Tests not compile
   (test-not-compile "adder-illtyped"
                     "../adder.rkt"
                     "#t"
                     #rx"#%datum: expected exact-nonnegative-integer\n  at: #t")

   (test-not-compile "adder-nonnegative"
                     "../adder.rkt"
                     "-1"
                     #rx"#%datum: expected exact-nonnegative-integer\n  at: -1")

   (test-not-compile "adder-bad-syntax"
                     "../adder.rkt"
                     "add1(42)")

   (test-not-compile "adder-bad-syntax"
                     "../adder.rkt"
                     "(add1)")

   (test-not-compile "adder-bad-syntax"
                     "../adder.rkt"
                     "(add 42)")

   ))

(module+ test (run-tests adder-tests))
