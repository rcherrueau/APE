#lang racket/base

(require "utils.rkt")

;; Lambda Calculus + Reference
;;
;; Expression
;; e ::= x             (Variable)
;;     | (λ x e)       (Abstraction)
;;     | (e e)         (Application)
;;     | r             (Mutable Reference)
;;
;; Mutable Reference
;; r ::= (ref e)       (Reference)
;;     | (set! e e)    (Assignment)
;;     | (deref e)     (Dereference)



(extends-lang "lam.rkt")

;; Reference
(define-syntax-rule (lam+ref-ref EXP)
  (box EXP))

;; Assignment
(define-syntax-rule (lam+ref-set! VAR EXP)
  (set-box! VAR EXP))

;; Dereference
(define-syntax-rule (lam+ref-deref VAR)
  (unbox VAR))

(provide (rename-out [lam+ref-ref ref]
                     [lam+ref-deref deref]
                     [lam+ref-set! set!]))
