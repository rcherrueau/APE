#lang racket

(require "print-test.rkt")


;; (do ([i 0 (add1 i)]
;;      [j 0 (add1 j)])
;;     ((> (+ i j) 10))
;;   (printf "i:~s " i)
;;   (printf "j:~s\n" j))

;; (let doloop ([i 0]
;;              [j 0])
;;   (when (not (> (+ i j) 10))
;;     (printf "i:~s " i)
;;     (printf "j:~s\n" j)
;;     (doloop (add1 i) (add1 j))))

;; (displayln
;;  (do ([i 0 (add1 i)]
;;       [j 0 (add1 j)])
;;      ((> (+ i j) 10) (format "i+j:~s" (+ i j)))
;;    (printf "i:~s " i)
;;    (printf "j:~s\n" j)))

;; (displayln
;;  (let doloop ([i 0]
;;               [j 0])
;;    (if (> (+ i j) 10)
;;        (format "i+j:~s" (+ i j))
;;        (begin
;;          (printf "i:~s " i)
;;          (printf "j:~s\n" j)
;;          (doloop (add1 i) (add1 j))))))

(define-syntax (do2 orig-x)
  (syntax-case orig-x ()
    [(_ ([var init . step] ...) (test e0 ...) c ...)
     ; step is not mandatory
     (with-syntax ([(step ...)
                    (map (lambda (v s)
                           (syntax-case s ()
                             [() v]
                             [(s) #'s]))
                         (syntax->list #'(var ...))
                         (syntax->list #'(step ...)))])
       (syntax-case #'(e0 ...) ()
         [()
          #'(let doloop ([var init] ...)
              (when (not test)
                (begin c ... (doloop step ...))))]
         [(e0 e1 ...)
          #'(let doloop ([var init] ...)
              (if test
                  (begin e0 e1 ...)
                  (begin c ... (doloop step ...))))]))]))

(print-test (do2 ([i 0 (add1 i)]
                  [j 0 (add1 j)])
                 ((> (+ i j) 10))
              (printf "i:~s " i)
              (printf "j:~s\n" j))

            (displayln
             (do ([i 0]
                  [j 0 (add1 j)])
                 ((> (+ i j) 10) (format "i+j:~s" (+ i j)))
               (printf "i:~s " i)
               (printf "j:~s\n" j))))
