;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "print-test.rkt")

;; `cond_3' recurs internally so that it always has a handle on the
;; original input expression `orig-x'.
(define-syntax (cond_3 orig-x)
  (let docond ([x orig-x])
    (syntax-case x (else =>)
      [(_)
       #'(void)]
      [(_ [else e1 e2 ...])
       #'(begin e1 e2 ...)]
      [(_ [test => func])
       #'(let ([t test]) (when t (func t)))]
      [(_ [test => func] c1 c2 ...)
       (with-syntax ([rest (docond #'(cond_3 c1 c2 ...))])
       #'(let ([t test]) (if t (func t) rest)))]
      [(_ [test])
       #'(let ([t test]) (when t t))]
      [(_ [test] c1 c2 ...)
       (with-syntax ([rest (docond #'(cond_3 c1 c2 ...))])
         #'(let ([t test]) (if t t rest)))]
      [(_ [test e1 e2 ...])
       #'(let ([t test]) (when t (begin e1 e2 ...)))]
      [(_ [test e1 e2 ...] c1 c2 ...)
       (with-syntax ([rest (docond #'(cond_3 c1 c2 ...))])
         #'(if test (begin e1 e2 ...) rest))])))

(print-test (cond_3)

            (cond_3
             [(member 2 '(1 2 3))])

            (cond_3
             [(zero? -5)]
             [(positive? -5)]
             [(positive? 5)])

            (cond_3
             [else 5 6 7 8])

            (cond_3
             [(positive? -5) (error "doesn't get there")]
             [(zero? -5) (error "doesn't get here, either")]
             [(positive? 5) 'here])

            (cond_3
             [(member 2 '(1 2 3)) => (lambda (l) (map - l))])

            (cond_3
             [(member 9 '(1 2 3)) => (lambda (l) (map - l))]
             [else 5]))
