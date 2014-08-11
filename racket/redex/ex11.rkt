#lang racket

(require redex)

;;-------------------------------------------------------------- Utils
;; Reduction sequence printer.
;;
;; /Note/ this function print one reduction. To see all availbale
;; possibilities, see `traces`.
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


;;------------------------------------------------------ Exercise 11.1
(term (+ ,(first (term (,(+ 12 34)))) 5))
;;> '(+ 46 5)

;;------------------------------------------------------ Exercise 11.2
(newline)
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


;;------------------------------------------------------ Exercise 11.3
(newline)
;; modulo 3 (%₃) semantics:
;; | + | 0 | 1 | 2 |
;; |---+---+---+---|
;; | 0 | 0 | 1 | 2 |
;; | 1 | 1 | 2 | 0 |
;; | 2 | 2 | 0 | 1 |
;;
;; (+ 0 A) %₃ A                 ;; [a]
;;
;; (+ A 0) %₃ A                 ;; [b]
;;
;; (+ 1 1) %₃ 1                 ;; [c]
;;
;; (+ 1 2) %₃ 0                 ;; [d]
;;
;;  (+ 1 (+ 1 A')) %₃ (+ 2 A')
;; ---------------------------- ;; [e]
;;    (+ 1 A) %₃ (+ 2 A')
;;
;;  (+ 1 (+ 2 A')) %₃ A'
;; ----------------------       ;; [f]
;;    (+ 1 A) %₃ A'
;;
;; (+ 2 1) %₃ 0                 ;; [g]
;;
;; (+ 2 2) %₃ 1                 ;; [h]
;;
;;  (+ 2 (+ 1 A')) %₃ A'
;; ----------------------       ;; [i]
;;     (+ 2 A) %₃ A'
;;
;;  (+ 2 (+ 2 A')) %₃ (+ 1 A')
;; ---------------------------- ;; [j]
;;   (+ 2 A) %₃ (+ 1 A')

(define %₃
  (reduction-relation
   simple-add-lang
   (--> (+ 0 A) A "[a]")
   (--> (+ A 0) A "[b]")
   (--> (+ 1 1) 2 "[c]")
   (--> (+ 1 2) 0 "[d]")
   (--> (+ 1 (+ 1 A)) (+ 2 A) "[e]")
   (--> (+ 1 (+ 2 A)) A "[f]")
   (--> (+ 2 1) 0 "[g]")
   (--> (+ 2 2) 1 "[h]")
   (--> (+ 2 (+ 1 A)) A "[i]")
   (--> (+ 2 (+ 2 A)) (+ 1 A) "[j]")))
(define %₃-red-seq (reduction-sequence %₃ "%₃"))

(displayln "Reduction sequence for %₃ relation:")
(%₃-red-seq (term (+ 1 0)))
(%₃-red-seq (term (+ 1 (+ 2 0))))
(%₃-red-seq (term (+ 2 (+ 1 (+ 2 0)))))

(displayln (string-append "Unfortunately, the %₃ relation doesn't "
                          "support the reduction of sub-expressions:"))
(%₃-red-seq (term (+ (+ 2 1) 1)))

(displayln (string-append "If we wish to reduce such expressions, we "
                          "must extend the %₃ relation to ->%₃, its "
                          "*compatible closure*:"))

;; The %₃ relation doesn't reduce expression like `(+ (+ 2 1) 1)`. If
;; we wish to reduce (+ (+ 2 1) 1) we must extend %₃ to its
;; *compatible closure* that supports the reduction of
;; sub-expressions.
;;
;;  B₁ ->%₃ B₂                 if B₁ %₃ B₂    [a]
;;  (+ B₁ B₂) ->%₃ (+ B₁' B₂)  if B₁ ->%₃ B₁' [b]
;;  (+ B₁ B₂) ->%₃ (+ B₁ B₂')  if B₂ ->%₃ B₂' [c]
;;
;; In [b], B₁ should be reduce to B₁'. In that case, B₁ is called the
;; *redex*. All parts which surrounds the redex, e.g.: `(• _ B₂)` is
;; called the *context*. Thus, the compatible closure is a single step
;; reduction within a *context*

;; Specification of the single step reduction within a *context* is
;; simply the %₃ relation surrounded by its context. The general idea
;; is the following: In C, `hole` is a redex, what we want in [b] and
;; [c] is applying the %₃ relation to the redex. To do that, first you
;; have to say where is your redex in the context grammar `C`. Then,
;; apply each rule of %₃ relation on C.

(define ->%₃
  (reduction-relation
   simple-add-lang
   (--> (in-hole C (+ 0 A))
        (in-hole C A)
        "[a]")
   (--> (in-hole C (+ A 0))
        (in-hole C A)
        "[b]")
   (--> (in-hole C (+ 1 1))
        (in-hole C 2)
        "[c]")
   (--> (in-hole C (+ 1 2))
        (in-hole C 0)
        "[d]")
   (--> (in-hole C (+ 1 (+ 1 A)))
        (in-hole C (+ 2 A))
        "[e]")
   (--> (in-hole C (+ 1 (+ 2 A)))
        (in-hole C A)
        "[f]")
   (--> (in-hole C (+ 2 1))
        (in-hole C 0)
        "[g]")
   (--> (in-hole C (+ 2 2))
        (in-hole C 1)
        "[h]")
   (--> (in-hole C (+ 2 (+ 1 A)))
        (in-hole C A)
        "[i]")
   (--> (in-hole C (+ 2 (+ 2 A)))
        (in-hole C (+ 1 A))

        "[j]")))

(define ->%₃-red-seq (reduction-sequence ->%₃ "->%₃"))
(->%₃-red-seq (term (+ (+ 2 1) 1)))
(->%₃-red-seq (term (+ (+ 2 (+ 2 (+ 2 1))) (+ 2 (+ 1 (+ 2 1))))))
(traces ->%₃ (term (+ (+ 2 (+ 2 (+ 2 1))) (+ 2 (+ 1 (+ 2 1))))))
