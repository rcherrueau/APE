#lang racket

(require (for-syntax racket/base)
         racket/match
         syntax/parse/define
         typed/racket
         "utils.rkt")

;; Adder -- A starter language that add number.
;;
;; See:
;; - https://course.ccs.neu.edu/cs4410/lec_let-and-stack_notes.html
;; - https://course.ccs.neu.edu/cs4410/hw_02_assignment.html

;; n  ∈ Nat
;;
;; exp ::=                (Exp)
;;         n              (Num)
;;       -- New in adder
;;       | (add1 exp)     (Prim1 Add1)
;;       | (sub1 exp)     (Prim1 Sub1)


;; Parser
(require (only-in "ast.rkt" Exp Num Prim1 Add1 Sub1))

;; Reuse neonate parser
(extends-lang "neonate.rkt")

;; Prim1 Add1
(define-syntax-parser add1
  [(_ EXP:expr) #'(Prim1 (Add1) EXP)])

;; Prim1 Sub1
(define-syntax-parser sub1
  [(_ EXP:expr) #'(Prim1 (Sub1) EXP)])


;; Compiler
(require "asm.rkt")

;; Take an Exp and compiles into a list of assembly Instructions.
(: compile-exp (Exp -> ASM))
(define (compile-exp exp)
  (exp⇒asm exp

    [(Prim1 (Add1) e)
     => ;; Compiles expression `e`; Result is in EAX.
        (compile-exp e)
        ;; Add 1 to EAX
        (Add (Reg (EAX)) (Const 1))]

    [(Prim1 (Sub1) e)
     => ;; Compiles expression `e`; Result is in EAX.
        (compile-exp e)
        ;; Add 1 to EAX
        (Sub (Reg (EAX)) (Const 1))]

    ;; -- Definitions from "neonate.rkt"
    [(Num n) => (Move (Reg (EAX)) (Const n))]
    [else (error "Compilation Error: Unsupported Exp" exp)]))


;; Interface
(provide add1 sub1 compile-exp)
