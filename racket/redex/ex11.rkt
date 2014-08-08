#lang racket

(require redex)

;;------------------------------------------------------ Exercise 11.1
(term (+ ,(first (term (,(+ 12 34)))) 5))
;;> '(+ 46 5)


;;------------------------------------------------------ Exercise 11.2
(define-language simple-add-lang
  ;; Addition of number
  [A 0         ;; [a]
     1         ;; [b]
     2         ;; [c]
     (+ A A)]  ;; [d]
  [C hole      ;; [e]
     (+ C A)   ;; [f]
     (+ A C)]) ;; [g]

(redex-match simple-add-lang
             ;; [C (+ 1 A)   ;; [e]
             ;;    (+ C A)   ;; [f]
             ;;    (+ A C)]) ;; [g]
             (in-hole C (+ 1 A))
             (term (+ 1 (+ 1 0))))

;;> (match (list (bind 'A 0) (bind 'C '(+ 1 hole))))
;;                      0 ∈ A [a]
;;                  --------------- [e]
;;   1 ∈ A [b]         (+ 1 0) ∈ C
;;  -------------------------------------- [g]
;;           (+ 1 (+ 1 0)) ∈ C
;;

;;> (match (list (bind 'A '(+ 1 0)) (bind 'C hole))))
;;
;;    1 ∈ A [b]   0 ∈ A [a]
;;   ---------------------- [d]
;;         (+ 1 0) ∈ A
;;  -------------------------- [e]
;;        (+ 1 (+ 1 0)) ∈ C
