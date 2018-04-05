#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         "asm.rkt"
         "utils.rkt")

(provide (rename-out [neonate-md #%module-begin]
                     [neonate-datum #%datum])
         #%top #%app #%top-interaction
         compile-exp)

;; A First Compiler -- Neonate. See,
;; https://course.ccs.neu.edu/cs4410/lec_neonate_notes.html
;;
;; n  ∈ Nat
;;
;; exp = n (Num)

(require (only-in "ast.rkt"
                  Exp
                  Num))


;; Parser -- This language assumes an s-exp reader

;; A module is an `EXP`, not the traditional list of `EXP` (ie,
;; EXP...). In other word, it ensures that only one program could be
;; given instead of a list of programs.
;;
;; The module compiles the underlining expressions with `compile-exp`.
;; Function `compile-exp` takes an `EXP` and produces an `ASM` (ie,
;; List of ASM instructions). It then gives the compiled expression to
;; `asm->string` that converts the `ASM` into a textual form.
(define-syntax (neonate-md stx)
  (syntax-case stx ()
    [(_ EXP)
     ;; By default, racket macro are hygienic. That is, racket
     ;; determines the binding of `compile-exp` identifier where the
     ;; macro is defined (aka, definition site). But I wanna define
     ;; new languages by reusing part of this language and extends
     ;; `compile-exp` at the place of the new language. Thus I want to
     ;; determine the binding of `compile-exp` at the place where the
     ;; macro is invoked (aka, calling site).
     ;;
     ;; To determine the binding of a identifier, racket looks inside
     ;; the /lexical context/. Thus, we should tell racket to create
     ;; the `compile-exp` identifier with the lexical context of the
     ;; new language definition.
     ;;
     ;; The `datum->syntax` function converts its second argument to a
     ;; syntax object by using the lexical context of its first
     ;; argument. Here, I create the `compile-exp` identifier with the
     ;; lexical context of `EXP`. Considering that `EXP` refers to a
     ;; `module` of the new language, the following does the trick.
     ;;
     ;; See: https://beautifulracket.com/explainer/hygiene.html
     (with-syntax ([COMPILE-EXP (datum->syntax #'EXP 'compile-exp)])
       #'(#%module-begin
          (provide ast asm)

          (define ast EXP)               ;; The AST of the Program
          (define asm (COMPILE-EXP ast)) ;; The ASM of the Program
          (printf (asm->string asm))     ;; Print textual form of ASM
          ))]))

;;  Only Nat are valid program
(define-syntax (neonate-datum stx)
 (syntax-parse stx
   [(_ . N:nat) #'(Num (#%datum . N))]))


;; Compiler

;; Takes a number and compiles it.
;; (: compile-exp (Exp -> ASM))
(define (compile-exp exp)
  (match exp
    [(Num n)
     (list (Move (Reg (EAX)) (Const n)))]))
