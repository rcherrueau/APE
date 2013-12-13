;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 15,16
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "print-test.rkt")

;; Commonize the various clauses by determining the "tail" of the
;; generated `if' expression before contructing the output.
(define-syntax (cond_4 x)

  ;; Construct the "tail" part of a cond, if any
  (define (cond-if-tail clauses)
    (syntax-case clauses ()
      [() #'((void))]
      [(c1 c2 ...) #'((cond_4 c1 c2 ...))]))

  (syntax-case x (else =>)
    [(_)
     #'(void)]
    [(_ [else e1 e2 ...])
     #'(begin e1 e2 ...)]
    ;; Commonize the (cond [test => func] ...)
    [(_ [test => func] c ...)
     (with-syntax ([tail (cond-if-tail #'(c ...))])
       #'(let ([t test]) (if t (func t) . tail)))]
    ;; Commonize the (cond [test] ...)
    [(_ [test] c ...)
     (with-syntax ([tail (cond-if-tail #'(c ...))])
       #'(let ([t test]) (if t t . tail)))]
    ;; Commonize the (cond [test ...] ...)
    [(_ [test e1 e2 ...] c ...)
     (with-syntax ([tail (cond-if-tail #'(c ...))])
       #'(if test (begin e1 e2 ...) . tail))]))

(print-test (cond_4)

            (cond_4
             [(member 2 '(1 2 3))])

            (cond_4
             [#f '42])

            (cond_4
             [(zero? -5)]
             [(positive? -5)]
             [(positive? 5)])

            (cond_4
             [else 5 6 7 8])

            (cond_4
             [(positive? -5) (error "doesn't get there")]
             [(zero? -5) (error "doesn't get here, either")]
             [(positive? 5) 'here])

            (cond_4
             [(member 2 '(1 2 3)) => (lambda (l) (map - l))])

            (cond_4
             [(member 9 '(1 2 3)) => (lambda (l) (map - l))]
             [else 5]))
