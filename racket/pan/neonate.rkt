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
;; exp = n

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
     (with-syntax ([COMPILE-EXP (datum->syntax #'EXP 'compile-exp)])
       #'(#%module-begin (printf (asm->string (COMPILE-EXP EXP)))))
     ]
    )
  )

;;  Only Nat are valid program
(define-syntax (neonate-datum stx)
 (syntax-parse stx
   [(_ . N:nat) #'(Num (#%datum . N))]))


;; Compiler

;; Takes a number and compiles it.
;;
;; (: compile-exp (Exp -> ASM))
(define (compile-exp exp)
  (match exp
    [(Num n)
     (list (Move (Reg (EAX)) (Const n)))]))
