;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 17,18
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "print-test.rkt")

;; Cond below expands into a pair of mutually recursive local macros
;; that perform the expension. This definition is much less efficient
;; that the previous version, sinc it requires two new macro
;; transformers to be constructed and evaluated each time a `cond'
;; expression occurs in a program.
;;
;; This definition demonstrates how local macro definitions may be
;; inserted into the output of a macro to perform additional comple
;; processing of the input.
(define-syntax (cond_7 x)
  (syntax-case x ()
    [(_ xc1 xc2 ...)
     ;; dots is used to introduce ellipses into the generated macros.
     (with-syntax ([dots #'(... ...)])
       #'(letrec-syntax
             ([cond1 (lambda (x)
                       (syntax-case x (else =>)
                         [(_)
                          #'(void)]
                         [(_ [else e1 e2 dots])
                          #'(begin e1 e2 dots)]
                         [(_ [test => func] c dots)
                          #'(let ([t test])
                              (cond2 t (func t) c dots))]
                         [(_ [test e1 e2 dots] c dots)
                          #'(cond2 test
                                   (begin e1 e2 dots)
                                   c dots)]
                         [(_ [test] c dots)
                          #'(let ([t test])
                              (cond2 t t c dots))]))]
              ;; Construct the "tail" part of the cond, if any
              [cond2 (lambda (x)
                       (syntax-case x ()
                         [(_ test e)
                          #'(when test e)]
                         [(_ test e c1 c2 dots)
                          #'(if test e
                                     (cond1 c1 c2 dots))]))])
           (cond1 xc1 xc2 ...)))]))

(print-test
            ;; ;; FIXME: that case not work
            ;; (cond_7)

            (cond_7
             [(member 2 '(1 2 3))])

            (cond_7
             [#f '42])

            (cond_7
             [(zero? -5)]
             [(positive? -5)]
             [(positive? 5)])

            (cond_7
             [else 5 6 7 8])

            (cond_7
             [(positive? -5) (error "doesn't get there")]
             [(zero? -5) (error "doesn't get here, either")]
             [(positive? 5) 'here])

            (cond_7
             [(member 2 '(1 2 3)) => (lambda (l) (map - l))])

            (cond_7
             [(member 9 '(1 2 3)) => (lambda (l) (map - l))]
             [else 5]))
