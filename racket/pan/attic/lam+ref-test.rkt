#lang s-exp "lam+ref.rkt"


;; Lambda Tests

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

;; SEQ
(λ x (λ y ((λ _ y) x)))

;; ;; LOOP
;; ((λ x (x x)) (λ x (x x)))


;; Mutable Reference Tests

(λ v (ref v))

(λ r (deref r))

(λ r (λ v (set! r v)))

;; SET ; DEREF
(λ r (λ v (((λ x (λ y ((λ _ y) x))) (set! r v)) (deref r))))

;; MAKEREF ; SET ; DEREF (hygienic on `v`)
(λ v ((λ r (λ v (((λ x (λ y ((λ _ y) x))) (set! r v)) (deref r)))) (ref v)))
