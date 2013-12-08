;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 13
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

(define-syntax (cond_1 x)
  ;; else and => in the literals list (fenders) informs that when
  ;; these identifiers appear in the pattern, they are literals rather
  ;; than pattern variables.
  (syntax-case x (else =>)
    ;; Order of the clauses is important. In general, more specific
    ;; patterns must appear before more general patterns that might
    ;; also match the same input.
    [(_)
     #'(void)]
    [(_ [else e1 e2 ...])
     #'(begin e1 e2 ...)]
    [(_ [test => func])
     #'(let ([t test]) (when t (func t)))]
    [(_ [test => func] c1 c2 ...)
     #'(let ([t test]) (if t (func t) (cond_1 c1 c2 ...)))]
    [(_ [test])
     #'(let ([t test]) (when t t))]
    [(_ [test] c1 c2 ...)
     #'(let ([t test]) (if t t (cond_1 c1 c2 ...)))]
    [(_ [test e1 e2 ...])
     #'(let ([t test]) (when t (begin e1 e2 ...)))]
    [(_ [test e1 e2 ...] c1 c2 ...)
     #'(let ([t test]) (if t (begin e1 e2 ...) (cond_1 c1 c2 ...)))]))

(print-test (cond_1)

            (cond_1
             [(member 2 '(1 2 3))])

            (cond_1
             [(zero? -5)]
             [(positive? -5)]
             [(positive? 5)])

            (cond_1
             [else 5 6 7 8])

            (cond_1
             [(positive? -5) (error "doesn't get there")]
             [(zero? -5) (error "doesn't get here, either")]
             [(positive? 5) 'here])

            (cond_1
             [(member 2 '(1 2 3)) => (lambda (l) (map - l))])

            (cond_1
             [(member 9 '(1 2 3)) => (lambda (l) (map - l))]
             [else 5]))
