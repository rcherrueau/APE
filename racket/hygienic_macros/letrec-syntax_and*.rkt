;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 7-8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "print-test.rkt")

; Local macro using let-syntax and letrec-syntax.
; Here, `letREC-syntax` form is important because macro is
; recursively-defined. `and*` macro appears in the `syntax` form of
; the last clause. For this reason, `letREC-syntax` has to be used
; instead of `let-syntax`
(letrec-syntax ([and* (lambda (x)
                        (syntax-case x ()
                          [(_) #'#t]
                          ; Base case clause.
                          [(_ e) #'e]
                          ; Recursive case clause. See the `and*` in
                          ; the syntax form
                          [(_ e1 e2 e3 ...)
                           #'(if e1 (and* e2 e3 ...) #f)]))])
  (let ([x '(a b c d)])
    (print-test
     (and* (pair? x) (pair? (cddr x)) (pair? (cdddr x)) (cadddr x))))
  (print-test
   (and* #t #f #f #t #t #t #f)
   (and* #t #t #t)
   (and*)))
