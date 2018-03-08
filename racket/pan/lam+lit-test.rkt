#lang s-exp "lam+lit.rkt"


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


;; Literal Tests

;; IFTHENELSE TRUE #t #f
((((λ test (λ if-true (λ if-false ((test if-true) if-false))))
   (λ t (λ f t))) #t) #f)

;; IFTHENELSE FALSE #t #f
((((λ test (λ if-true (λ if-false ((test if-true) if-false))))
   (λ t (λ f f))) #t) #f)
