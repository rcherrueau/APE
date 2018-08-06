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


;; Parser -- This language assumes an s-exp reader
(require "ast.rkt")

;; A module is an `EXP`, not the traditional list of `EXP` (ie,
;; EXP...). In other word, it ensures that only one program could be
;; given instead of a list of programs.
;;
;; The module compiles the underlining expressions with `compile-exp`.
;; Function `compile-exp` takes an `EXP` and produces an `ASM` (ie,
;; List of ASM instructions). It then gives the compiled expression to
;; `asm->string` that converts the `ASM` into a textual form.
(define-syntax (@%module-begin stx)
  (syntax-parse stx
    [(_ EXP:expr)
     #'(racket/base/#%module-begin
        (provide ast asm)              ;; Expose `ast` and `asm` out
                                       ;; for unit tests.

        (define ast EXP)               ;; The AST of the Program
        (define asm (compile-exp ast)) ;; The ASM of the Program
        (printf (asm->string asm))     ;; Print textual form of ASM
        )]))

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
(provide (rename-out [@%module-begin #%module-begin]
                     [@%datum #%datum])
         #%app #%top-interaction compile-exp)
