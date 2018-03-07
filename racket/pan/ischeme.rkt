#lang racket/base

(require "utils.rkt")

;; Idealized Scheme

;; x ∈ Vars
;; c ∈ Consts

;; e ::= v | x | r | (e e) | (ρ (θ) e)                    (Expressions)
;; r ::= (ref e) | (deref e) | (set! x e)                 (Reference)
;; v ::= c | (λ x e)                                      (Values)
;; θ ::= ε | θ(x v), where (x v') ∉ θ                     (ρ-lists)
;; a ::= v | (ρ (θ) v)                                    (Answers)


;; Macro Expander

(extends-lang "lam.rkt")

;; List of Idealized Scheme Expressions
(define-syntax-rule (ischeme-mb ISCHEME-EXP ...)
  (#%module-begin ISCHEME-EXP ...))

;; Reference
(define-syntax-rule (ischeme-ref EXP)
  (box EXP))

;; Assignment
(define-syntax-rule (ischeme-set! VAR EXP)
  (set-box! VAR EXP))

;; Dereference
(define-syntax-rule (ischeme-deref EXP)
  (unbox EXP))

;; Block
(define-syntax-rule (ischeme-ρ ([VAR VAL] ...) EXP)
  (letrec ([VAR VAL] ...) EXP))

(provide (rename-out [ischeme-mb #%module-begin]
                     [ischeme-ref ref]
                     [ischeme-deref deref]
                     [ischeme-set! set!]
                     [ischeme-ρ ρ]
                     ))

(module+ test
  (require rackunit)

  ;; ----------------------------------- from lam.rkt
  (define TRUE  (lam:λ t (lam:λ f t)))
  (define FALSE (lam:λ t (lam:λ f f)))
  (define IFTHENELSE
    (lam:λ test
           (lam:λ if-true
                  (lam:λ if-false
                         (lam:#%app (lam:#%app test if-true)
                                    if-false)))))

  (check-true (lam:#%app (lam:#%app (lam:#%app IFTHENELSE TRUE) #t) #f))
  (check-false (lam:#%app (lam:#%app (lam:#%app IFTHENELSE FALSE) #t) #f))

  ;; ----------------------------------- specific
  (check-equal? (ischeme-ρ ([x 10]) x) 10)
  )
