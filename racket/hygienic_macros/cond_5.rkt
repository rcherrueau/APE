;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 16
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "print-test.rkt")

;; Construct the tail before even looking at the current clause, and
;; uses simplified patterns to be matched only against the current
;; clause.
(define-syntax (cond_5 orig-x)
  (syntax-case orig-x ()
    [(_ test c ...)
     (with-syntax ([tail
                    (syntax-case #'(c ...) ()
                      [() #'((void))]
                      [(c1 c2 ...) #'((cond_5 c1 c2 ...))])])
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
    [(_) #'(void)]))

(print-test (cond_5)

            (cond_5
             [(member 2 '(1 2 3))])

            (cond_5
             [#f '42])

            (cond_5
             [(zero? -5)]
             [(positive? -5)]
             [(positive? 5)])

            (cond_5
             [else 5 6 7 8])

            (cond_5
             [(positive? -5) (error "doesn't get there")]
             [(zero? -5) (error "doesn't get here, either")]
             [(positive? 5) 'here])

            (cond_5
             [(member 2 '(1 2 3)) => (lambda (l) (map - l))])

            (cond_5
             [(member 9 '(1 2 3)) => (lambda (l) (map - l))]
             [else 5]))
