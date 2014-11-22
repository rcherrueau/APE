;; Applications of Continuations: Invited Tutorial - Fri88
;; Daniel P. Friedman
;; http://www.cs.indiana.edu/~dfried/appcont.pdf
#lang racket/base

(require (for-syntax racket))

;; Application of Continuations
;; Daniel P. Friedman
;; 1988


;; ----------------------------------------------------------- Definition
;; A very simple example of call/cc
(+ (call/cc
    (λ (k^) (/ (k^ 5) 4)))
   8)
;; 1. k^ = (λ (v) (+ v 8))
;; 2. (/ (k^ 5) 4)
;; 3. (k^ 5)
;; 4. 13
;; The wainting division (2) is forgotten since k^ is an escape proc


;; -----------------------------------------------------------------------
;; Continuation describes the rest of the computation
(* (+ (call/cc
       (λ (k^) (/ (k^ 5) 4)))
      8)
   3)
;; 1. k^ = (λ (v) (* (+ v 8) 3))
;; 2. (/ (k^ 5) 4)
;; 3. (k^ 5)
;; 4. 39
;; The waiting division (2) is forgotten since k^ is an escape proc

;; /The rest of the computation/ is not always obvious
(* (+ (let ([u (+ 3 2)])
        (call/cc
         (λ (j^) (/ (j^ u) 4))))
      8)
   3)
;; Determining the value for `u` is priore to determining the value
;; for `j^`.
;; 1. j^ = (λ (v) (* (+ v 8) 3))
;; 2. (/ (j^ u) 4)
;; 3. (j^ u)
;; 4. (j^ 5)
;; 5. 39


;; -----------------------------------------------------------------------
;; Continuations are first-class objects
(define +8^ void)

(+ (call/cc
    (λ (k^)
       (begin
         (set! +8^ k^)
         (displayln "inside body")
         5)))
   8)
;; 1. k^ = (λ (v) (+ v 8))
;; 2. (begin (set! +8^ k^) (displayln "inside body") 5)
;;    The variable `+8^` is set to the continuation `k^`. "inside
;;    body" is printed. And the λ returns `5`.

;; Then the program should be stopped here because we never call the
;; continuation. But, you have to keep in mind that:
;; (call/cc (λ (k^) e)) = (call/cc (λ (k^) (k^ e)))
;; This is why we surround e with `begin`.
;; Thus,
;; 3. (+ 8 5)
;; 4. 13
;; Here we save the continuation, but not invoke it.

;; Later we may invoke it
(* (/ (+8^ 35) 0) 100)
;; 1. +8^ = (λ (v) (+ v 8))
;; 2. (* (/ (+8^ 35) 0) 100)
;; 3. (/ (+8^ 35) 0)
;; 4. (+8^ 35)
;; 5. 43
;; The waiting division (3) and multiplication (4) is forgotten since
;; `+8^` is an escape proc.


;; ----------------------------------------------------- Meta-Programming
;; Default behaviour of `call/cc`
;; Remeber that:
;; (call/cc (λ (k^) e)) = (call/cc (λ (k^) (k^ e)))


;; -----------------------------------------------------------------------
;; λ^
;;
;; REMINDER: λ^ is not a built in construction parts of Scheme/Racket.
;; See "How to construct λ^ below"
;;
;; Escaping by applying `call/cc` to an escape procedure offers to the
;; user more control. The `call/cc` with an *unsecaped lambda*
;; automatically applies the continuation with the body of that lambda
;; (see, Default behaviour of `call/cc`). The `call/cc` with and
;; *escaped lambda* lets the developer control the application of the
;; continuation:
;; (+ 3 (call/cc (λ^ (k^) (k^ 8)))) ; => 11 explicite calls
;; (+ 3 (call/cc (λ^ (k^) 8))) ; => 8 no calls


;; -----------------------------------------------------------------------
;; How to construct λ^
;;
;; (λ^ (id ...) e ...) ≡ (λ (id ...) (INVOKE/NO-CONT (lambde () e ...)))
;;
;; With `INVOKE/NO-CONT` (i.e.: invoke without continuation) an escape
;; procedure without parameter. Thus the body `e ...' is executed
;; outside of the continuation.
(define INVOKE/NO-CONT #f)

(define (make-INVOKE/NO-CONT)
  ((call/cc (λ (k^)
               (set! INVOKE/NO-CONT (λ (th) (k^ th)))
               (λ () 'INVOKE/NO-CONT)))))
(make-INVOKE/NO-CONT)

(define-syntax (λ^ stx)
  (syntax-case stx ()
    [(_ (id ...) e ...)
     #'(lambda (id ...) (INVOKE/NO-CONT (lambda () e ...)))]))

;; We want an escape procedure that after execution, escapes to the
;; top level. So, what we do there? We know that with `call/cc` we
;; could register a continuation. Thus, with `make-INVOKE/NO-CONT' we
;; register the continuation *of the top level* in a variable
;; `INVOKE/NO-CONT'. Finally, with our syntax `λ^' we execute the body
;; of the `λ^' in the `INVOKE/NO-CONT' escape prrocedure which escapes
;; at the top level.

;; -----------------------------------------------------------------------
;; A simple LISP-like `BREAK`
;;
;; Idea: My BREAK save the continuation and print the message or do
;; some control. And then I can resume my continuation. To do so, in
;; the BREAK I save the current continuation (what surround the
;; `BREAK`) in a RESUME. Then, I can call the RESUM to resume the
;; continuation.
(define RESUME #f)

(define (BREAK message)
  (call/cc (λ (k^)
              (set! RESUME k^)
              ((λ^ (x) x) message))))

(* (BREAK (+  5 8)) 3)
;; > (+ 5 8)
;; > 13
(displayln "BREAK returns to top level")
(RESUME 13)
;; > (* 13 3)
;; > 39

;; For sequences, we have to put theme in a let, or define, or lambda
;; instead of `begin'. The behaviour of continuation in a begin is
;; that begin always splices . See,
;; http://lists.scheme-reports.org/pipermail/scheme-reports/2013-April/003292.html
(let ()
  (displayln "1")
  (BREAK (displayln "2"))
  (displayln "3")
  (displayln "4"))
(displayln "BREAK returns to top level")
(RESUME)


;; -----------------------------------------------------------------------
;; Programming in continuation-passing-style
;; Binary search tree
(struct bst (info left right))

;; Times nodes of the tree. Idea: If a 0 is found in the tree, the
;; result is 0 whatever the value of remaining nodes.
(define (times-bst tree)
  (call/cc
   (λ (exit^)
      (let times-bst1 ([t tree])
        (cond
         [(null? t) 1]
         [(zero? (bst-info t)) (exit^ 0)]
         [else
          (* (bst-info t)
             (times-bst1 (bst-left t))
             (times-bst1 (bst-right t)))])))))
(times-bst (bst 3 (bst 2 null null) (bst 3 null null)))
(times-bst (bst 0 (bst 2 null null) (bst 3 null null)))

;; Let's do the same thing in continuation-passing-style
(define (times-bst-k tree)
  (let times-bst-k1 ([t tree]
                     [k (λ (x) x)])
    (cond
     [(null? t) (k 1)]
     [(zero? (bst-info t)) 0]
     [else
      (times-bst-k1 (bst-left t)
                    (λ (res1)
                       (times-bst-k1 (bst-right t)
                                     (λ (res2)
                                        (k (* (bst-info t)
                                              res1
                                              res2))))))])))
(times-bst-k (bst 3 (bst 2 null null) (bst 3 null null)))
(times-bst-k (bst 0 (bst 2 null null) (bst 3 null null)))


;; -----------------------------------------------------------------------
;; Meta-Programming with `call/cc`: CYCLE
(define (CYCLE f)
  (call/cc (λ (k^)
              (let loop ()
                (f k^)
                (loop)))))

(displayln (let ([n 10])
            (CYCLE (λ (EXIT-CICLE-WITH)
                      (cond
                       [(zero? n) (EXIT-CICLE-WITH "end-loop")]
                       [else (set! n (sub1 n))
                             (display "loop")])))))
