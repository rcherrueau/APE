;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "print-test.rkt")

; Must return the value of the first subexpression that evaluate to a
; true (nonfalse) value.
(define-syntax (or* x)
  (syntax-case x ()
    [(_) #'#f]
    [(_ e) #'e]
    [(_ e1 e2 e3 ...)
     ; If there is more than one subexpression, it must both test the
     ; value of the first subexpression and return its value if it
     ; does evaluate to a true value. In order to avoid evaluating the
     ; subexpression twice, a temporary variable `t` is introduced.
     #'(let ([t e1]) (if t t (or* e2 e3 ...)))]))

(print-test (or*)
            (or* #f #f #f)
            (or* #f #f #f '42 #f))
