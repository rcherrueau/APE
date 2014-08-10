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

(displayln "Test term ∈ B?")
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

(displayln (string-append "Redex (• true (• true false)) matching "
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
(define r
  (reduction-relation
   bool-any-lang
   (--> (• false B) B a)
   (--> (• true B) true b)))

(define (reduction-sequence r r-name)
  (define (_red-seq term [nest 0])
    (define rts (apply-reduction-relation r term))
    (cond
     [(null? rts)
      (cond
       [(equal? nest 0)
        (printf "No reduction for ~a~n" term)]
       [else
        (void)])]
     [else
      (cond
       [(equal? nest 0)
        (define red-term (car rts))
        (define new-nest (string-length (format "~a" term)))
        (printf "~a ~a ~a~n" term r-name red-term)
        (_red-seq red-term new-nest)]
       [else
        (define red-term (car rts))
        (printf "~a ~a ~a~n"
                (make-string nest #\space)
                r-name
                red-term)
        (_red-seq red-term nest)])]))
  _red-seq)

(displayln "Reduction sequence based on the r one-step relation:")
(define r-red-seq (reduction-sequence r "r"))
(for ([t (list (term (• false (• false (• true false)))) B1 B2 B5)])
  (printf "Reduction of ~a:~n" t)
  (r-red-seq t))
(newline)

;; The r relation doesn't reduce expression like `true`, `false` or
;; `(• (• true false) false))`. What we need is the compatible closure
;; which is a single step reduction within a *context*.

;; Specification of the single step reduction within a *context*.
(displayln "Reduction sequence based on the r one-step relation:")
(define ->r
  (reduction-relation
   bool-any-lang
   (-->  (in-hole C (• true B))
         (in-hole C true)
         b)
   (-->  (in-hole C (• false B))
         (in-hole C B)
         c)))

(traces ->r B5)
