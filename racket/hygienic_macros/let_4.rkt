;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "print-test.rkt")

; `all-ids' fender reject input expression that do not have
; identifiers where they are expected.
(define-syntax (let_4 x)
  (define (all-ids? lst)
    (define (id? an-id previous-id-res)
      (and (identifier? an-id) previous-id-res))
    (foldl id? #t (syntax->list lst)))

  (syntax-case x ()
    [(_ ([i v] ...) e1 e2 ...)
     (all-ids? #'(i ...))
     #'((lambda (i ...) e1 e2 ...) v ...)]
    [(_ name ([i v] ...) e1 e2 ...)
     (all-ids? #'(i ...))
     #'((letrec ((name (lambda (i ...) e1 e2 ...))) name)
        v ...)]))

(print-test (let_4 ([v1 '1]
                    [v2 '3]
                    [v3 '3]
                    [v4 '7])
                   (format "~s~s~s~s" v1 v2 v3 v4))

            (let_4 fac ([n 10])
                   (if (zero? n)
                       1
                       (* n (fac (sub1 n)))))
            ; "v1" as identifier will report a bad syntax error.
            (let_4 (["v1" '1])
                   (format "~s" v1)))
