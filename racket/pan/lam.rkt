#lang racket/base

;; Lambda Calculus

;; e ::=              Expression:
;;      x             Variable
;;    | (λ x e)       Abstraction
;;    | (e e)         application



;; Macro Expander

(define-syntax-rule (lam-mb EXP ...)     ;; List of Expression
  (#%module-begin EXP ...))

(define-syntax-rule (lam-app EXP1 EXP2)  ;; Application
  (#%app EXP1 EXP2))

(define-syntax-rule (lam-abs VAR EXP)    ;; Abstraction
  (λ (VAR) EXP))

(provide (rename-out [lam-mb #%module-begin]
                     [lam-app #%app]
                     [lam-abs λ])
         #%datum ;; Provides Racket values (e.g., #t, 42, "donatello")
         quote   ;; Provides Racket symbol
         )

(module+ test
  (require rackunit)

  (define TRUE  (lam-abs t (lam-abs f t)))
  (define FALSE (lam-abs t (lam-abs f f)))
  (define IFTHENELSE
    (lam-abs test
             (lam-abs if-true
                      (lam-abs if-false
                               (lam-app (lam-app test if-true)
                                        if-false)))))

  (check-true (lam-app (lam-app (lam-app IFTHENELSE TRUE) #t) #f))
  (check-false (lam-app (lam-app (lam-app IFTHENELSE FALSE) #t) #f)))
