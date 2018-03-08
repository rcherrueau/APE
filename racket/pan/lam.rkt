#lang racket/base

;; Lambda Calculus

;; Expression
;; e ::= x             (Variable)
;;    | (λ x e)       (Abstraction)
;;    | (e e)         (Application)



(define-syntax-rule (lam-mb EXP ...)     ;; List of Expression
  (#%module-begin EXP ...))

(define-syntax-rule (lam-abs VAR EXP)    ;; Abstraction
  (λ (VAR) EXP))

(define-syntax-rule (lam-app EXP1 EXP2)  ;; Application
  (#%app EXP1 EXP2))

(define-syntax-rule (lam-var VAR)        ;; Variable
  (#%variable-reference VAR))

(provide (rename-out [lam-mb #%module-begin]
                     [lam-var #%variable-reference]
                     [lam-abs λ]
                     [lam-app #%app]
                     ))
