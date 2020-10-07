#lang racket/base

(require racket/list
         cKanren/miniKanren
         cKanren/neq)

;; ---------------------------------------------------------
;; utils

;; The label next to `l` is `l+1`.
(define (l+1o l l+1)
  ;; Numeric values in miniKanren are represented as Oleg
  ;; numerals—little-endian lists of binary digits that encode the
  ;; base-2 representation of the number (from left to right base-2
  ;; representation). They should be build with `build-num`
  ;;
  ;; See tests/mk.rkt in miniKanren sources
  ;;
  ;; (run1 [q] (l+1o (build-num 1) (build-num 2))) ;; (_.0)
  ;; (run1 [q] (l+1o (build-num 1) q))             ;; ((0 1))
  ;; (run1 [q] (l+1o (build-num 1) (build-num 1))) ;; ()
  (pluso l (build-num 1) l+1))

;; The instruction `inst` at label `l` in program `prog`
(define (insto l prog inst)
  ;; (indexo l prog inst)
  (conde
   ;; l: x = f y z
   [(fresh [x y z]
      (membero `(f/2 ,l ,x ,y ,z) prog)
      (== `(f/2 ,l ,x ,y ,z) inst))]
   ;; l: x = <const>
   [(fresh [x]
      (membero `(cst ,l ,x) prog)
      (== `(cst ,l ,x) inst))]
   ;; l: return x
   [(fresh [x]
      (membero `(ret ,l ,x) prog)
      (== `(ret ,l ,x) inst))]))

;; ---------------------------------------------------------
;; Live analysis -- version 1

;; Variable `var` is live at label `l` in program `prog`
(define (liveo_1 l var prog)
  (fresh [inst]
    (insto l prog inst)
    (conde
     ;; Rule 1
     [(fresh [x z]
        (== `(f/2 ,l ,x ,var ,z) inst))]
     [(fresh [x y]
        (== `(f/2 ,l ,x ,y ,var) inst))]
     ;; Rule 2
     [(fresh [l+1 x y z]
        (== `(f/2 ,l ,x ,y ,z) inst)
        (l+1o l l+1)
        (liveo_1 l+1 var prog)
        (=/= x var))]
     ;; Rule 3
     [(== `(ret ,l ,var) inst)]
     ;; Rule 4
     [(fresh [l+1 x]
        (== `(cst ,l ,x) inst)
        (l+1o l l+1)
        (liveo_1 l+1 var prog)
        (=/= x var))])))

;; ---------------------------------------------------------
;; Live analysis -- version 2

;; The variable `var` is define at line `l` in program prog.
(define (defo l var prog)
  (fresh [inst]
    (insto l prog inst)
    (conde
     ;; l: x = f y z
     [(fresh [y z]
        (== `(f/2 ,l ,var ,y ,z) inst))]
     ;; l: x = <const>
     [(== `(cst ,l ,var) inst)])))


;; The variable `var` is not define at line `l` in program prog.
;;
;; (run1 [q] (!defo (build-num 0) 'a `((ret ,(build-num 0) a))))     ;; #t
;; (run1 [q] (!defo (build-num 0) 'a `((cst ,(build-num 0) x))))   ;; #t
;; (run1 [q] (!defo (build-num 0) 'a `((cst ,(build-num 0) a))))   ;; #f
;; (run1 [q] (!defo (build-num 0) 'a `((f/2 ,(build-num 0) x a b)))) ;; #t
;; (run1 [q] (!defo (build-num 0) 'a `((f/2 ,(build-num 0) a a b)))) ;; #f
(define (!defo l var prog)
  (conde
   ;; There is a def at that position and the variable bind is
   ;; different than `var`.
   [(fresh [x]
      (defo l x prog)
      (=/= var x))]
   ;; The instruction at that line is not a definition line (i.e., a
   ;; return)
   [(fresh [inst x]
      (insto l prog inst)
      (== `(ret ,l ,x) inst))]))

;; The instruction at label `ll` is executed after `l` in the program.
;;
;; (run1 [q] (succo (build-num 0) (build-num 1) prog)) ;; #t
;; (run1 [q] (succo (build-num 2) (build-num 2) prog)) ;; #f
;; (run1 [q] (succo (build-num 5) (build-num 6) prog)) ;; #f -- no 6 label
(define (succo l ll prog)
  (fresh [inst/l inst/ll]
    ;; There exists an instruction at label `l`
    (insto l prog inst/l)
    ;; There exists an instruction at label `ll`
    (insto ll prog inst/ll)
    ;; Instruction at ll is l + 1
    (l+1o l ll)))

;; The variable `var` is used (read) at line `l` in program `prog`.
;;
;; (run1 [q] (useo (build-num 1) 'a prog))  ;; #t
;; (run1 [q] (useo (build-num 5) 'e prog))  ;; #t
;; (run1 [q] (useo (build-num 0) 'a prog))  ;; #f
(define (useo l var prog)
  (fresh [inst]
    (insto l prog inst)
    (conde
     ;; l: x = f y z
     [(fresh [x z] (== `(f/2 ,l ,x ,var ,z) inst))]
     [(fresh [x y] (== `(f/2 ,l ,x ,y ,var) inst))]
     ;; l: return x
     [(== `(ret ,l ,var) inst)])))

;; The variable `var` is live at line `l` in program `prog`.
;;
;; Only defined in terms of `useo`, `succo` and `defo`.
(define (liveo_2 l var prog)
  (conde
   [(useo l var prog)]
   [(fresh [ll]
      (succo l ll prog)
      (liveo_2 ll var prog)
      (!defo l var prog))]))

;; ---------------------------------------------------------
;; CLI

;; Display live variable of a program `prog` using analysis `analyze`.
(define (live-variables analyze prog)
  (printf "analysis ~a:~n" (object-name analyze))
  (for ([line (in-range (length prog))])
    (let* ([label (build-num line)]
           [vars (remove-duplicates (run* [q] (analyze label q prog)))])
      (printf "~a: ~a~n" line vars)))
  (printf "~n"))

;; Example program
(define prog `((cst ,(build-num 0) a)
               (f/2 ,(build-num 1) b a a)
               (f/2 ,(build-num 2) c b a)
               (f/2 ,(build-num 3) d a b)
               (f/2 ,(build-num 4) e d c)
               (ret ,(build-num 5) e)))

;; analysis liveo_1:
;; 0: ()
;; 1: (a)
;; 2: (b a)
;; 3: (a b c)
;; 4: (d c)
;; 5: (e)
(live-variables liveo_1 prog)

;; analysis liveo_2:
;; 0: ()
;; 1: (a)
;; 2: (b a)
;; 3: (a b c)
;; 4: (d c)
;; 5: (e)
(live-variables liveo_2 prog)
