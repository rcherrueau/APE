#lang s-exp "lam+ref+lit.rkt"


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


;; Literal Tests

;; IFTHENELSE TRUE #t #f
((((λ test (λ if-true (λ if-false ((test if-true) if-false))))
   (λ t (λ f t))) #t) #f)

;; IFTHENELSE FALSE #t #f
((((λ test (λ if-true (λ if-false ((test if-true) if-false))))
   (λ t (λ f f))) #t) #f)


;; Mutable Reference w/ Literal Tests

;; Provide ref to 10; set ref to 20; deref
(((λ r (λ v (((λ x (λ y ((λ _ y) x))) (set! r v)) (deref r)))) (ref 10)) 20)

;; Test hygienic
(((λ v ((λ r (λ v (((λ x (λ y ((λ _ y) x))) (set! r v)) (deref r)))) (ref v))) 10) 20)
