#lang racket/base

(require (prefix-in r: racket/base)
         "utils.rkt")

;; Lambda Calculus + Reference
;;
;; x ∈ Variables (a, b, c, ...)
;; l ∈ Literals (#t,#f,1,2,3,...,"Donatello")
;;
;; Expression
;; e ::= x             (Variable)
;;     | (λ x e)       (Abstraction)
;;     | (e e)         (Application)
;;     | l             (Literal)
;;     | r             (Mutable Reference)
;;
;; Mutable Reference
;; r ::= (ref e)       (Reference)
;;     | (set! e e)    (Assignment)
;;     | (deref e)     (Dereference)


(extends-lang "lam+lit.rkt")
(extends-lang "lam+ref.rkt")
