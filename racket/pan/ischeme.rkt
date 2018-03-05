#lang racket/base

;; Idealized Scheme

;; x ∈ Vars
;; c ∈ Consts

;; e ::= v | x | (e e) | (set! x e) | (ρ (θ) e) (Expressions)
;; v ::= c | (λ x e)                            (Values)
;; θ ::= ε | θ[x v], where (x v') ∉ θ           (ρ-lists)
;; r ::= v | (ρ (θ) v)                          (Results)


;; Expander
;;
(require (for-syntax racket/base))

(provide (rename-out [ischeme-module-begin #%module-begin])
         #%top #%app #%datum #%top-interaction)

(define-syntax (ischeme-module-begin stx)
  (syntax-case stx ()
    [(_ ISCHEME-EXP ...)
     #'(#%module-begin
        ISCHEME-EXP ...)]))

;; Assignment
(define-syntax (ischeme-set! stx)
  (syntax-case stx ()
    [(_ VAR EXP)
        #'(set! VAR EXP)]))

;; Block
(define-syntax (ρ stx)
  (syntax-case stx ()
    [(_ ([VAR VAL] ...) EXP)
     #'(letrec ([VAR VAL] ...) EXP)]
    ))

;; Abstraction
(define-syntax (λ stx)
  (syntax-case stx ()
    [(_ VAR EXP)
     #'(lambda (VAR) EXP)]
    ))


(provide (rename-out [ischeme-set! set!]) ρ λ)

(module+ test
  (ρ ([x 10]) x)
  )
