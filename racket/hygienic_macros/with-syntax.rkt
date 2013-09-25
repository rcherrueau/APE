;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "print-test.rkt")
; `with-syntax' creates a pattern variable bindingjto hold ;
; intermediate output within the body of the macro. `with-syntax';
; form for pattern wariable is similar to `let' for ordinary ;
; variables.
(define-syntax (with-syntax x)
  (syntax-case x ()
    [(_ ([p ctx] ...) e1 e2 ...)
     #'(syntax-case (list ctx ...) ()
         [(p ...) (begin e1 e2 ...)])]))

(define-syntax (or* x)
  (syntax-case x ()
    [(_) #'#f]
    [(_ e) #'e]
    [(_ e1 e2 e3 ...)
     ; `with-syntax' enables to build up the recursivity separately
     ; from the whole.
     (with-syntax [(rest #'(or* e2 e3 ...))]
       #'(let ([t e1]) (if t t (or* rest))))]))

(print-test (or*)
            (or* #f #f #f)
            (or* #f #f #f '42 #f))
