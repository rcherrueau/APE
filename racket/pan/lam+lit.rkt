#lang racket/base

(require "utils.rkt")

;; Lambda Calculus + Literals
;;
;; Expression
;; e ::= x             (Variable)
;;     | (λ x e)       (Abstraction)
;;     | (e e)         (Application)
;;     | l             (Literal)



(extends-lang "lam.rkt")

(define-syntax-rule (lam+lit-lit . LIT)     ;; Literals
  (#%datum . LIT))

(provide (rename-out [lam+lit-lit #%datum]))

(module+ test
  (require rackunit)

  (define TRUE  (lam:λ t (lam:λ f t)))
  (define FALSE (lam:λ t (lam:λ f f)))
  (define IFTHENELSE
    (lam:λ test
             (lam:λ if-true
                      (lam:λ if-false
                               (lam:#%app (lam:#%app test if-true)
                                        if-false)))))

  (check-true (lam:#%app (lam:#%app (lam:#%app IFTHENELSE TRUE) (lam+lit-lit . #t)) (lam+lit-lit . #f)))
  (check-false (lam:#%app (lam:#%app (lam:#%app IFTHENELSE FALSE) (lam+lit-lit . #t)) (lam+lit-lit . #f)))
  )
