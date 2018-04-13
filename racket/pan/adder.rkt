#lang racket

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in racket/base/ racket/base)
         racket/match
         "asm.rkt"
         "utils.rkt")

;; Adder -- A starter language that add number.
;;
;; See:
;; - https://course.ccs.neu.edu/cs4410/lec_let-and-stack_notes.html
;; - https://course.ccs.neu.edu/cs4410/hw_02_assignment.html

;; n  ∈ Nat
;;
;; exp ::= n              (Num)
;;       | (add1 e)       (Prim1 Add1)
;;       | (sub1 e)       (Prim1 Sub1)

(require (only-in "ast.rkt" Exp Num Prim1 Add1 Sub1))


;; Parser
(extends-lang "neonate.rkt")

(define-syntax-rule (add1 EXP)
  (Prim1 (Add1) EXP))

(define-syntax-rule (sub1 EXP)
  (Prim1 (Sub1) EXP))

(provide add1 sub1 compile-exp)


;; Compiler
;; (: compile-exp (Exp -> ASM))
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
