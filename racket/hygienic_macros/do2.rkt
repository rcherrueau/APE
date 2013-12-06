;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Hygienic Macros in Scheme with Syntax-case
;; R. Kent Dybvig
;; Technical Report #356
;;
;; page 13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "print-test.rkt")

;; if test? produces a true value, the the finish expressions, e.g.:
;; e1 ... are evaluated in order, and the last one is evaluated in
;; tail position to produce the overall value for the do form. If no
;; finish expressions are provided, the value of the do form is
;; #<void>

;; (do ([i 0 (add1 i)]
;;      [j 0 (add1 j)])
;;     ((> (+ i j) 10))
;;   (printf "i:~s " i)
;;   (printf "j:~s\n" j))
;;
;; (let doloop ([i 0]
;;              [j 0])
;;   (when (not (> (+ i j) 10))
;;     (printf "i:~s " i)
;;     (printf "j:~s\n" j)
;;     (doloop (add1 i) (add1 j))))
;;
;; (displayln
;;  (do ([i 0 (add1 i)]
;;       [j 0 (add1 j)])
;;      ([> (+ i j) 10]
;;       [format "i+j:~s"(+ i j)])
;;    (printf "i:~s " i)
;;    (printf "j:~s\n" j)))
;;
;; (let doloop ([i 0]
;;              [j 0])
;;   (if (> (+ i j) 10)
;;     ;; if this is the end of the loop, we return the value of the
;;     ;; finish expression
;;     (format "i+j:~s" (+ i j))
;;     ;; else, we conitnue the loop
;;     (begin
;;       (printf "i:~s " i)
;;       (printf "j:~s\n" j)
;;       (doloop (add1 i) (add1 j)))))

(define-syntax (do2 orig-x)
  (syntax-case orig-x ()
    [(_ ((var init . step) ...) (test? e1 ...) c ...)
     (with-syntax [(step ...)
                   (map (lambda (v s)) #'(var ...) #'(step ...))]
       (syntax-case #'(e1 ...) ()
         [()
          #'(let doloop ([var init] ...)
              (when (not test?)
                (begin c ... (doloop step ...))))]
         [(e1 e2 ...)
          #'(let doloop ([var init] ...)
              (if (test?)
                  (begin e1 e2 ...)
                  (begin c ... (doloop step ...))))]))]))
