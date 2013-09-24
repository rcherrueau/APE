;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "print-test.rkt")

; Checks let form not containing duplicate bound identifier using
; `check-duplicate-identifier'
(define-syntax (let_6 x)
  (define (unique-ids? lst)
     (not (check-duplicate-identifier (syntax->list lst))))

  (define (all-ids? lst)
    (define (id? an-id previous-id-res)
      (and (identifier? an-id) previous-id-res))
    (foldl id? #t (syntax->list lst)))

  (syntax-case x ()
    [(_ ([i v] ...) e1 e2 ...)
     (if (all-ids? #'(i ...))
         (if (unique-ids? #'(i ...))
             #'((lambda (i ...) e1 e2 ...) v ...)
             (raise-syntax-error #f "duplicate identifier found" x))
         (raise-syntax-error #f "non-identifier found" x))]
    [(_ name ([i v] ...) e1 e2 ...)
     (identifier? #'name)
     (if (all-ids? #'(i ...))
         (if (unique-ids? #'(i ...))
             #'((letrec ([name (lambda (i ...) e1 e2 ...)]) name)
                v ...)
             (raise-syntax-error #f "duplicate identifier found" x))
         (raise-syntax-error #f "non-identifier found" x))]))

(print-test (let_6 ([v1 '1]
                    [v2 '3]
                    [v3 '3]
                    [v4 '7])
                   (format "~s~s~s~s" v1 v2 v3 v4))

            (let_6 fac ([n 10])
                   (if (zero? n)
                       1
                       (* n (fac (sub1 n)))))

            ;; x raises a syntax error with message "duplicate
            ;; identifier found".
            (let_6 ([x '1]
                    [x '2])
                   (format "~s" x)))
