;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "print-test.rkt")

; Ellipse of pattern `[i v] ...` can be split up in the output as
; `i ...` and `v ...`
(define-syntax (let_1 x)
  (syntax-case x ()
    [(_ ([i v] ...) e1 e2 ...)
     #'((lambda (i ...) e1 e2 ...) v ...)]))

(print-test (let_1 ([v1 '4]
                    [v2 '2])
                   (printf "~a~a\n" v1 v2)))
