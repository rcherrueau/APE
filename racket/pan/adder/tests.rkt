#lang racket/base

(require rackunit
         rackunit/text-ui
         "ast.rkt"
         "utils.rkt")

(module+ test (run-tests
  (test-suite
   "adder lang tests"


   ;; Tests AST
   (test-ast "adder-42"
             "42"
             (Num 42))

   (test-ast "adder-add1-42"
             "(add1 42)"
             (Prim1 (Add1) (Num 42)))

   (test-ast "adder-sub1-42"
             "(sub1 42)"
             (Prim1 (Sub1) (Num 42)))

   (test-ast "adder-sub1-add1-add1-42"
             "(sub1 (add1 (add1 42)))"
             (Prim1 (Sub1) (Prim1 (Add1) (Prim1 (Add1) (Num 42)))))


   ;; Tests compile
   (test-compile "adder-42"
                  "42"
                  (unlines "    .intel_syntax noprefix"
                           "    .global _the_asm_code"
                           "    .text"
                           ""
                           "_the_asm_code:"
                           "    mov eax, 42"
                           "    ret"))

   (test-compile "adder-add1-42"
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
                     "#t"
                     #rx"#%datum: expected exact-nonnegative-integer\n  at: #t")

   (test-not-compile "adder-nonnegative"
                     "-1"
                     #rx"#%datum: expected exact-nonnegative-integer\n  at: -1")

   (test-not-compile "adder-bad-syntax"
                     "add1(42)")

   (test-not-compile "adder-bad-syntax"
                     "(add1)")

   (test-not-compile "adder-bad-syntax"
                     "(add 42)")

   )))
