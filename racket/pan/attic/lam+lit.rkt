#lang racket/base

(require (prefix-in r: racket/base)
         "utils.rkt")

;; Lambda Calculus + Literals
;;
;; x ∈ Variables (a, b, c, ...)
;; l ∈ Literals (#t,#f,1,2,3,...,"Donatello")
;;
;; Expression
;; e ::= x             (Variable)
;;     | (λ x e)       (Abstraction)
;;     | (e e)         (Application)
;;     | l             (Literal)


(extends-lang "lam.rkt")

;; Literals
(define-syntax-rule (#%datum . LIT)
  (r:#%datum . LIT))

(provide (all-defined-out))
