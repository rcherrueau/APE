#lang racket/base

(require (prefix-in r: racket/base))

;; Lambda Calculus
;;
;; x ∈ Variables (a, b, c, ...)
;;
;; Expression
;; e ::= x            (Variable)
;;    | (λ x e)       (Abstraction)
;;    | (e e)         (Application)


;; List of Expression
(define-syntax-rule (#%module-begin EXP ...)
  (r:#%module-begin EXP ...))

;; Abstraction
(define-syntax-rule (λ VAR EXP)
  (r:λ (VAR) EXP))

;; Application
(define-syntax-rule (#%app EXP1 EXP2)
  (r:#%app EXP1 EXP2))

(provide (all-defined-out))
