;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 14,15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "print-test.rkt")

;; ;; Input:
;; (cond)
;; ;; Output:
;; (void)

;; ;; Input:
;; (cond
;;  [(member 2 '(1 2 3))])
;; ;; Output:
;; (let ([t (member 2 '(1 2 3))])
;;   (when t t))

;; ;; Input:
;; (cond
;;  [(zero? -5)]
;;  [(positive? -5)]
;;  [(positive? 5)])
;; ;; Ouput:
;; (let ([t (zero? -5)])
;;   (if t t (cond
;;            [(positive? -5)]
;;            [(positive? 5)])))

;; ;; Input:
;; (cond
;;  [else 5 6 7 8])
;; ;; Output:
;; (begin 5 6 7 8)

;; ;; Input:
;; (cond
;;  [(positive? 5) (error "doesn't get there")]
;;  [(zero? -5) (error "doesn't get here, either")]
;;  [(positive? 5) 'here])
;; ;; Output:
;; (let ([t (positive? -5)])
;;   (if t
;;       (error "doesn't get there")
;;       (cond
;;        [(zero? -5) (error "doesn't get here, either")]
;;        [(positive? 5) 'here])))

;; ;; Input:
;; (cond
;;  [(member 2 '(1 2 3)) => (lambda (l) (map - l))])
;; ;; Output:
;; (let ([t (member 2 '(1 2 3))])
;;   (when t ((lambda (l) (map - l)) t)))

;; ;; Input:
;; (cond
;;  [(member 9 '(1 2 3)) => (lambda (l) (map - l))]
;;  [else 5])
;; ;; Output:
;; (let ([t (member 9 '(1 2 3))])
;;   (if t
;;       ((lambda (l) (map - l)) t)
;;       (cond
;;        [else 5])))

;; Use `free-identifier=?' in place of the `syntax-case' literals
;; list.
(define-syntax (cond_2 x)
  (syntax-case x ()
    [(_)
     #'(void)]
    [(_ [x e1 e2 ...])
     (and (identifier? #'x)
          (free-identifier=? #'x #'else))
     #'(begin e1 e2 ...)]
    [(_ [test x func])
     (and (identifier? #'x)
          (free-identifier=? #'x #'=>))
     #'(let ([t test]) (when t (func t)))]
    [(_ [test x func] c1 c2 ...)
     (and (identifier? #'x)
          (free-identifier=? #'x #'=>))
     #'(let ([t test]) (if t (func t) (cond_2 c1 c2 ...)))]
    [(_ [test])
     #'(let ([t test]) (when t t))]
    [(_ [test] c1 c2 ...)
     #'(let ([t test]) (if t t (cond_2 c1 c2 ...)))]
    [(_ [test e1 e2 ...])
     #'(let ([t test]) (when t (begin e1 e2 ...)))]
    [(_ [test e1 e2 ...] c1 c2 ...)
     #'(if test (begin e1 e2 ...) (cond_2 c1 c2 ...))]))

(print-test (cond_2)

            (cond_2
             [(member 2 '(1 2 3))])

            (cond_2
             [(zero? -5)]
             [(positive? -5)]
             [(positive? 5)])

            (cond_2
             [else 5 6 7 8])

            (cond_2
             [(positive? -5) (error "doesn't get there")]
             [(zero? -5) (error "doesn't get here, either")]
             [(positive? 5) 'here])

            (cond_2
             [(member 2 '(1 2 3)) => (lambda (l) (map - l))])

            (cond_2
             [(member 9 '(1 2 3)) => (lambda (l) (map - l))]
             [else 5]))
