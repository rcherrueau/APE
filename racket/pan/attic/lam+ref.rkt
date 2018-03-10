#lang racket/base

(require (prefix-in r: racket/base)
         "utils.rkt")

;; Lambda Calculus + Reference
;;
;; x ∈ Variables (a, b, c, ...)
;;
;; Expression
;; e ::= x             (Variable)
;;     | (λ x e)       (Abstraction)
;;     | (e e)         (Application)
;;     | r             (Mutable Reference)
;;
;; Mutable Reference
;; r ::= (ref e)       (Reference)
;;     | (set! x e)    (Assignment)
;;     | (deref x)     (Dereference)


(extends-lang "lam.rkt")

;; Reference
(define-syntax-rule (ref EXP)
  (r:box EXP))

;; Assignment
(define-syntax-rule (set! VAR EXP)
  (r:set-box! VAR EXP))

;; Dereference
(define-syntax-rule (deref VAR)
  (r:unbox VAR))

(provide (all-defined-out))
