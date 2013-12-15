;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 17
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "print-test.rkt")

;; Combines the techniques from `cond_3' (internally recursion) and
;; `cond_5' (tail construction before even looking at the clause).
(define-syntax (cond_6 orig-x)
  (let docond ([x orig-x])
    (syntax-case x ()
      [(_ test c ...)
       (with-syntax ([tail
                      (syntax-case #'(c ...) ()
                        [() #'((void))]
                        [(c1 c2 ...)
                         (with-syntax ([rest (docond #'(cond_6 c1 c2 ...))])
                           #'(rest))])])
         (syntax-case #'test (else =>)
           [(else e1 e2 ...)
            ;; tail has to be null in a `else' expression
            ;; `else' clause must be the last one.
            (when (not (eq? 'void
                            (syntax-e (car (syntax-e (car (syntax-e #'tail)))))))
              (raise-syntax-error #f "else clause must be the last one" #'test))

            #'(begin e1 e2 ...)]
           [(test => func)
            #'(let ([t test]) (if t (func t) . tail))]
           [(test)
            #'(let ([t test]) (if t t . tail))]
           [(test e1 e2 ...)
            #'(if test (begin e1 e2 ...) . tail)]))]
      [(_) #'(void)])))

(print-test (cond_6)

            (cond_6
             [(member 2 '(1 2 3))])

            (cond_6
             [#f '42])

            (cond_6
             [(zero? -5)]
             [(positive? -5)]
             [(positive? 5)])

            (cond_6
             [else 5 6 7 8])

            (cond_6
             [(positive? -5) (error "doesn't get there")]
             [(zero? -5) (error "doesn't get here, either")]
             [(positive? 5) 'here])

            (cond_6
             [(member 2 '(1 2 3)) => (lambda (l) (map - l))])

            (cond_6
             [(member 9 '(1 2 3)) => (lambda (l) (map - l))]
             [else 5]))
