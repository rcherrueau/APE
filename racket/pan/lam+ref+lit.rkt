#lang racket/base

(require "utils.rkt")

;; Lambda Calculus + Reference
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
