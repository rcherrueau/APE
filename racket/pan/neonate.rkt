#lang racket

(require (for-syntax racket/base)
         (prefix-in racket/base/ racket/base)
         racket/match
         syntax/parse/define
         typed/racket
         "utils.rkt")

;; A First Compiler -- Neonate. See,
;; https://course.ccs.neu.edu/cs4410/lec_neonate_notes.html
;;
;; n  ∈ Nat
;;
;; exp ::=   (Exp)
;;         n (Num)


;; Parser
(require (only-in "ast.rkt" Exp Num))

;; Reuse core parser
(extends-lang "core.rkt")

;; Num -- Only Nat are valid program
(define-syntax-parser @%datum
  [(_ . N:nat) #'(Num (racket/base/#%datum . N))])


;; Compiler
(require "asm.rkt")

;; Takes a number and compiles it.
(: compile-exp (Exp -> ASM))
(define (compile-exp exp)
  (exp⇒asm exp
    [(Num n) => (Move (Reg (EAX)) (Const n))]
    [else (error "Compilation Error: Unsupported Exp" exp)]))


;; Interface
(provide (rename-out [@%datum #%datum]) compile-exp)
