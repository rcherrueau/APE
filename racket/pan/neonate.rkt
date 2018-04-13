#lang racket

(require (for-syntax racket/base)
         (prefix-in racket/base/ racket/base)
         racket/match
         syntax/parse/define
         "asm.rkt"
         "utils.rkt")

;; A First Compiler -- Neonate. See,
;; https://course.ccs.neu.edu/cs4410/lec_neonate_notes.html
;;
;; n  ∈ Nat
;;
;; exp = n (Num)

(require (only-in "ast.rkt" Exp Num))


;; Parser
(extends-lang "core.rkt")

;;  Only Nat are valid program
(define-syntax-parser neonate-#%datum
  [(_ . N:nat) #'(Num (racket/base/#%datum . N))])

(provide (rename-out [neonate-#%datum #%datum]) compile-exp)


;; Compiler

;; Takes a number and compiles it.
;; (: compile-exp (Exp -> ASM))
(define (compile-exp exp)
  (exp⇒asm exp
    [(Num n) => (Move (Reg (EAX)) (Const n))]
    [else (error "Compilation Error: Unsupported Exp" exp)]))
