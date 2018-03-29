#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "asm.rkt")

(provide (rename-out [int-md #%module-begin]
                     [int-datum #%datum])
         #%top #%app #%top-interaction
         compile-exp)

;; A First Compiler -- Neonate. See,
;; https://course.ccs.neu.edu/cs4410/lec_neonate_notes.html
;;
;; n  ∈ Nat
;;
;; exp = n


;; Parser -- This language assumes an s-exp reader

;; A module is an `EXP`, not the traditional list of `EXP` (ie,
;; EXP...). In other word, it ensures that only one program could be
;; given instead of a list of programs.
;;
;; The module compiles the underlining expressions with `compile-exp`.
;; Function `compile-exp` takes an `EXP` and produces an `ASM` (ie,
;; List of ASM instructions). It then gives the compiled expression to
;; `asm->string` that converts the `ASM` into a textual form.
(define-syntax-rule (int-md EXP)
   (#%module-begin (printf (asm->string (compile-exp EXP)))))

;;  Only Nat are valid program
(define-syntax (int-datum stx)
 (syntax-parse stx
   [(_ . N:nat) #'(#%datum . N)]))


;; Compiler

;; Takes a number and compiles it.
;;
;; (: compile-exp (AST -> ASM))
(define (compile-exp number)
  (list (Move (Reg (EAX)) (Const number))))
