;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 9,10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "print-test.rkt")

; To avoid unamed `let' to match pattern for named `let' (because of
; the order of clauses), we use a fender. The fender uses the
; `identifier?' predicate to ensure that name is an identifier.
(define-syntax (let_3 x)
  (syntax-case x ()
    [(_ name ([i v] ...) e1 e2 ...)
     (identifier? #'name)
     #'((letrec ((name (lambda (i ...) e1 e2 ...))) name)
        v ...)]
    [(_ ([i v] ...) e1 e2 ...)
     #'((lambda (i ...) e1 e2 ...) v ...)]))

(print-test (let_3 ([v1 '4]
                    [v2 '2])
                   (format "~s~s" v1 v2))

            (let_3 fac ([n 10])
                   (if (zero? n)
                       1
                       (* n (fac (sub1 n))))))
