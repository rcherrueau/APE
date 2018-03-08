#lang s-exp "lam.rkt"

;; True
(λ t (λ f t))

;; False
(λ t (λ f f))

;; IFTHENESLE
(λ test (λ if-true (λ if-false ((test if-true) if-false))))

;; Y Combinator
(λ g ((λ x (g (x x))) (λ x (g (x x)))))

;; ISZERO
(λ n (n ((λ x (λ t (λ f f))) (λ t (λ f t)))))

;; ;; LOOP
;; ((λ x (x x)) (λ x (x x)))
