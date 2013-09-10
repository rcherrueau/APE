;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "print-test.rkt")

; Defines named `let' form in terms of letrec. Be careful about the
; order of the two clauses. A named `let' could never match pattern of
; the first clause but the inverse is not true. A unamed `let' could
; match the named pattern, since the pattern variable `name' matches
; anything. To avoid unamed `let' to match named `let', we declare
; unamed `let' as the first pattern to match. The thrid version of
; `let', e.g. `let_3', correct this problem.
(define-syntax (let_2 x)
  (syntax-case x ()
    [(_ ([i v] ...) e1 e2 ...)
     #'((lambda (i ...) e1 e2 ...) v ...)]
    [(_ name ([i v] ...) e1 e2 ...)
     #'((letrec ((name (lambda (i ...) e1 e2 ...))) name)
        v ...)]))

(print-test (let_2 ([v1 '4]
                    [v2 '2])
                   (printf "~a~a\n" v1 v2))

            (let_2 fac ([n 10])
                   (if (zero? n)
                       1
                       (* n (fac (sub1 n))))))
