#lang racket

(require redex)

(define-language bool-any-lang
  ;; Boolean
  [B true      ;; True       [a]
     false     ;; False      [b]
     (• B B)]  ;; Logical Or [c]
  ;; Context
  [C hole      ;; [d]
     (• C B)   ;; [e]
     (• B C)]) ;; [f]

(define B1 (term true))
(define B2 (term false))
(define B3 (term (• true false)))
(define B4 (term (• ,B1 ,B2)))
(define B5 (term (• false ,B4)))

;; Boolean expression with context.
;; Boolean expression could appear in the hole.
;; Evaluation depends on the context, i.e.: what "fill the hole".
;; You can fill the hole with `in-hole` in an expression.
(define C1 (term hole))
(define C2 (term (• (• false false) hole)))
(define C3 (term (• hole true)))

;; Use `redex-match` tool in conjonction with the grammar to
;; demonstrate that an expression belongs to the grammar (Do the same
;; jobs as a derivation tree for the ∈ relation).
(define (∈-B? term)
  (and (redex-match bool-any-lang B term) #t))

(for ([t (list B1 B2 B3 B4 B5 "hello")])
  (cond
   [(∈-B? t)
    (printf "~a ∈ B~n" t)]
   [else
    (printf "~a ∉ B~n" t)]))


;; A pattern may match an expression in several different ways.
;; Consider the example of matching a filled context with an
;; expression.
(redex-match bool-any-lang
             ;; Fill the hole with `(• true B)`. That gives the
             ;; language:
             ;; [C' (• true B)   ; [d']
             ;;     (• C' B)     ; [e']
             ;;     (• B C')]    ; [f']
             (in-hole C (• true B))
             (term (• true (• true false))))

;; Produces two matches where each refers to a derivation tree:
;;
;; (match (list (bind 'B 'false) (bind 'C '(• true hole)))):
;;
;;                        false ∈ B [b]
;;                   --------------------- [d']
;;  true ∈ B [a]      (• true false) ∈ C'
;; ---------------------------------------- [f']
;;       (• true (• true false)) ∈ C'
;;
;; (match (list (bind 'B '(• true false)) (bind 'C hole)))):
;;
;;   true ∈ B [a]   false ∈ B [b]
;;  ------------------------------ [c]
;;        (• true false) ∈ B
;; ----------------------------------- [d']
;;     (• true (• true false)) ∈ C'
