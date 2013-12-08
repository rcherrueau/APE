;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "print-test.rkt")

;; Input:
;; (letrec ([is-even? (lambda (n)
;;                      (or (zero? n)
;;                          (is-odd? (sub1 n))))]
;;          [is-odd? (lambda (n)
;;                     (and (not (zero? n))
;;                          (is-even? (sub1 n))))])
;;   (is-odd? 11))
;;
;; Output:
;; (let ([is-even? #f]
;;       [is-odd? #f])
;;   (let ([tmp1 (lambda (n)
;;                      (or (zero? n)
;;                          (is-odd? (sub1 n))))]
;;         [tmp2 (lambda (n)
;;                     (and (not (zero? n))
;;                          (is-even? (sub1 n))))])
;;     (set! is-even? tmp1)
;;     (set! is-odd? tmp2)
;;     (is-odd? 11)))

;; Use with-syntax to bind pattern variables to output from help
;; functions.
(define-syntax (letrec2 x)
  (syntax-case x ()
    [(_ ([i v] ...) e1 e2 ...)
     (with-syntax ([(t ...) (generate-temporaries #'(i ...))])
       #'(let ([i #f] ...)
           (let ([t v] ...)
             (set! i t) ...
             e1 e2 ...)))]))

(define (sub2 n) (- n 2))

(print-test (letrec2 ([is-even? (lambda (n)
                                 (or (zero? n)
                                     (is-odd? (sub1 n))))]
                     [is-odd? (lambda (n)
                                (and (not (zero? n))
                                     (is-even? (sub1 n))))])
              (is-odd? 11))

            (letrec2 ([fibonacci (lambda (n)
                                   (if (< n 2)
                                       n
                                       (+ (fibonacci (sub1 n))
                                          (fibonacci (sub2 n)))))])
                     (fibonacci 25)))
