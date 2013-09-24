;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 10,11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "print-test.rkt")

; Move `all-ids?' test into the output expression and call
; `raise-syntax-error'
(define-syntax (let_5 x)
  (define (all-ids? lst)
    (define (id? an-id previous-id-res)
      (and (identifier? an-id) previous-id-res))
    (foldl id? #t (syntax->list lst)))

  (syntax-case x ()
    [(_ ([i v] ...) e1 e2 ...)
     (if (all-ids? #'(i ...))
         #'((lambda (i ...) e1 e2 ...) v ...)
         (raise-syntax-error #f "non-identifier found" x))]
    [(_ name ([i v] ...) e1 e2 ...)
     (identifier? #'name)
     (if (all-ids? #'(i ...))
         #'((letrec ([name (lambda (i ...) e1 e2 ...)]) name)
            v ...)
         (raise-syntax-error #f "non-identifier found" x))]))

(print-test (let_5 ([v1 '1]
                    [v2 '3]
                    [v3 '3]
                    [v4 '7])
                   (format "~s~s~s~s" v1 v2 v3 v4))

            (let_5 fac ([n 10])
                   (if (zero? n)
                       1
                       (* n (fac (sub1 n)))))

            ; "v1" raises a syntax error with message "non-identifier
            ; found".
            (let_5 (["v1" '1])
                   (format "~s" v1)))
