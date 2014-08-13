#lang racket

(require redex
         "utils.rkt")

(define-language bool-any-lang
  ;; Boolean
  [B true      ;; True       [a]
     false     ;; False      [b]
     (• B B)]  ;; Logical Or [c]
  ;; Context
  [C hole      ;;            [d]
     (• C B)   ;;            [e]
     (• B C)]) ;;            [f]

(define B1 (term true))
(define B2 (term false))
(define B3 (term (• true false)))
(define B4 (term (• ,B1 ,B2)))
(define B5 (term (• ,B4 false)))

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

(displayln "* Test term ∈ B?")
(for ([t (list B1 B2 B3 B4 B5 "hello")])
  (cond
   [(∈-B? t)
    (printf "~a ∈ B~n" t)]
   [else
    (printf "~a ∉ B~n" t)]))
(newline)


;; A pattern may match an expression in several different ways.
;; Consider the example of matching a filled context with an
;; expression.
(displayln (string-append "* Redex (• true (• true false)) matching "
                          "while context is filling with "
                          "(• true B): "))
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
(newline)


;; Specification of the single step reduction with *no context*.
(displayln "* Reduction sequence based on the r one-step relation:")
(define r
  (reduction-relation
   bool-any-lang
   (--> (• false B) B "[a]")
   (--> (• true B) true "[b]")))

(define r-red-seq (reduction-sequence r "r"))
(for ([t (list (term (• false (• false (• true false)))) B5)])
  (printf "Reduction of ~a:~n" t)
  (r-red-seq t))
(newline)


;; The r relation doesn't reduce expression like `(• (• true false)
;; false))`. If we wish to reduce (• (• f t) f) we must extend r to
;; its *compatible closure* that supports the reduction of
;; sub-expressions.
;;
;;  B₁ ->r B₂                 if B₁ r B₂    [a]
;;  (• B₁ B₂) ->r (• B₁' B₂)  if B₁ ->r B₁' [b]
;;  (• B₁ B₂) ->r (• B₁ B₂')  if B₂ ->r B₂' [c]
;;
;; In [b], B₁ should be reduce to B₁'. In that case, B₁ is called the
;; *redex*. All parts which surrounds the redex, e.g.: `(• _ B₂)` is
;; called the *context*. Thus, the compatible closure is a single step
;; reduction within a *context*

;; Specification of the single step reduction within a *context* is
;; simply the r relation surrounded by its context. The general idea
;; is the following: In C, `hole` is a redex, what we want in [b] and
;; [c] is applying the r relation to the redex. To do that, first you
;; have to say where is your redex in the context grammar `C`. Then,
;; apply each rule of r relation on C.
(displayln (string-append "* Reduction sequence based on the ->r "
                          "single-step within a context relation:"))
(define ->r
  (reduction-relation
   bool-any-lang
   (-->  (in-hole C (• false B))
         (in-hole C B) "[a]")
   (-->  (in-hole C (• true B))
         (in-hole C true) "[b]")))

(define ->r-red-seq (reduction-sequence ->r "->r"))
(for ([t (list (term (• false (• false (• true false)))) B5)])
  (printf "Reduction of ~a:~n" t)
  (->r-red-seq t))
(newline)


;; The reduction sequence only prints one from many reduction
;; sequences. To see all reduction sequences, Redex provides a tool
;; called traces.
; (traces ->r (term (• (• true false) (• true true))))


;; Evaluation context for leftmost-outermost reduction strategy.
(define-language bool-standard-lang
  ;; Boolean
  [B true      ;; True       [a]
     false     ;; False      [b]
     (• B B)]  ;; Logical Or [c]
  ;; Evaluation context
  [E (• E B)   ;;            [d]
     hole])    ;;            [e]

(define ->re
  (reduction-relation
   bool-standard-lang
   (-->  (in-hole E (• true B))
         (in-hole E true) "[b]")
   (-->  (in-hole E (• false B))
         (in-hole E B) "[a]")))

(define ->re-red-seq (reduction-sequence ->re "->re"))
(traces ->re (term (• (• true false) (• true true))))
