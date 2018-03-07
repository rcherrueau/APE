#lang s-exp "lam.rkt"

;; True
(λ t (λ f t))

;; False
(λ t (λ f f))

;; IFTHENESLE
(λ test (λ if-true (λ if-false ((test if-true) if-false))))

;; IFTHENELSE TRUE #t #f
((((λ test (λ if-true (λ if-false ((test if-true) if-false))))
   (λ t (λ f t))) 't) 'f)

;; IFTHENELSE FALSE #t #f
((((λ test (λ if-true (λ if-false ((test if-true) if-false))))
   (λ t (λ f f))) 't) 'f)

;; ;; loop
;; ((λ x (x x)) (λ x (x x)))
