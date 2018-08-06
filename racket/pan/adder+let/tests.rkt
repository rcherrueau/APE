#lang racket/base

(require rackunit
         rackunit/text-ui
         "ast.rkt"
         "utils.rkt")


(module+ test (run-tests
  (test-suite
   "adder+let lang tests"


   ;; Tests AST
   (test-ast "adder+let-42"
             "42"
             (Num 42))

   (test-ast "adder+let-add1-42"
             "(add1 42)"
             (Prim1 (Add1) (Num 42)))

   (test-ast "adder+let-sub1-42"
             "(sub1 42)"
             (Prim1 (Sub1) (Num 42)))

   (test-ast "adder+let-sub1-add1-add1-42"
             "(sub1 (add1 (add1 42)))"
             (Prim1 (Sub1) (Prim1 (Add1) (Prim1 (Add1) (Num 42)))))

   (test-ast "adder+let-letx"
             "(let ([x 10]) (add1 x))"
             (Let (list [cons 'x (Num 10)]) (Prim1 (Add1) (Id 'x))))

   (test-ast "adder+let-letxy"
             "(let ([x 10] [y (add1 x)]) (sub1 y))"
             (Let (list [cons 'x (Num 10)]
                        [cons 'y (Prim1 (Add1) (Id 'x))])
                  (Prim1 (Sub1) (Id 'y))))


   ;; Tests compile
   (test-compile "adder+let-42"
                  "42"
                  (unlines "    .intel_syntax noprefix"
                           "    .global _the_asm_code"
                           "    .text"
                           ""
                           "_the_asm_code:"
                           "    mov eax, 42"
                           "    ret"))

   (test-compile "adder+let-add1-42"
                 "(add1 42)"
                 (unlines "    .intel_syntax noprefix"
                          "    .global _the_asm_code"
                          "    .text"
                          ""
                          "_the_asm_code:"
                          "    mov eax, 42"
                          "    add eax, 1"
                          "    ret"))

   (test-compile "adder+let-sub1-42"
                 "(sub1 42)"
                 (unlines "    .intel_syntax noprefix"
                          "    .global _the_asm_code"
                          "    .text"
                          ""
                          "_the_asm_code:"
                          "    mov eax, 42"
                          "    sub eax, 1"
                          "    ret"))

   (test-compile "adder+let-sub1-add1-add1-42"
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

   (test-compile "adder+let-letx"
                 "(let ([x 10]) (add1 x))"
                 (unlines "    .intel_syntax noprefix"
                          "    .global _the_asm_code"
                          "    .text"
                          ""
                          "_the_asm_code:"
                          "    mov eax, 10"
                          "    mov [esp - 4*1], eax"
                          "    mov eax, [esp - 4*1]"
                          "    add eax, 1"
                          "    ret"))

   (test-compile "adder+let-letxy"
                 "(let ([x 10] [y (add1 x)]) (sub1 y))"
                 (unlines "    .intel_syntax noprefix"
                          "    .global _the_asm_code"
                          "    .text"
                          ""
                          "_the_asm_code:"
                          "    mov eax, 10"
                          "    mov [esp - 4*1], eax"
                          "    mov eax, [esp - 4*1]"
                          "    add eax, 1"
                          "    mov [esp - 4*2], eax"
                          "    mov eax, [esp - 4*2]"
                          "    sub eax, 1"
                          "    ret"))


   ;; Tests not compile
   (test-not-compile "adder+let-illtyped"
                     "#t"
                     #rx"#%datum: expected exact-nonnegative-integer\n  at: #t")

   (test-not-compile "adder+let-nonnegative"
                     "-1"
                     #rx"#%datum: expected exact-nonnegative-integer\n  at: -1")

   (test-not-compile "adder+let-bad-syntax"
                     "add1(42)")

   (test-not-compile "adder+let-bad-syntax"
                     "(add1)")

   ;; With the new 'Id, `add` is parsed as `(Id 'add)`
   (test-not-compile "adder+let-bad-syntax"
                     "(add 42)"
                     #rx"application: not a procedure")

   (test-not-compile "adder+let-unbound-id"
                     "(let ([x 10]) (add1 y))"
                     #rx"hash-ref: no value found for key\n  key: 'y")

   (test-not-compile "adder+let-letxy"
                     "(let ([x 10] [x 10]) 42)"
                     #rx"let: duplicate identifier found")
   )))
