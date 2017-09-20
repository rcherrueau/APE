#lang racket/base

;; From http://beautifulracket.com

;; BrainFuck lang Grammar
;;
;; bf-prog ::= (bf-op | bf-loop)*
;; bf-op   ::= '>' | '<' | '+' | '-' | '.' | ','
;; bf-loop ::= '[' (bf-op | bl-loop)* ']'


;; Parser
;;
(require megaparsack megaparsack/text
         data/applicative data/monad)

;; Returns #t if `c' is one of ><+-.,[]
;; :: char → bool
(define (bf-token? c)
  (or (char=? #\> c)
      (char=? #\< c)
      (char=? #\+ c)
      (char=? #\- c)
      (char=? #\. c)
      (char=? #\, c)
      (char=? #\[ c)
      (char=? #\] c)))

;; Util parser combinator that consumes chars that are not tokens.
;; :: parser → parser
(define (bf-read/p p)
  (define goto-next-token/p
    (many/p (satisfy/p (compose1 not bf-token?))))

  (do goto-next-token/p
      [r <- p]
      goto-next-token/p
      (pure r)))

;; Parser that reads bf-op
;; bf-op ::= '>' | '<' | '+' | '-' | '.' | ','
(define bf-op/p
  (bf-read/p (do
       [x <- (char-in/p "><+-.,")]
       (pure `(bf-op ,x)))))

;; Parser that reads bf-loop
;; bf-loop ::= '[' (bf-op | bl-loop)* ']'
(define bf-loop/p
  (bf-read/p (do
      (char/p #\[)
      [x <- (many/p (or/p bf-op/p bf-loop/p))]
      (char/p #\])
      (pure `(bf-loop ,x)))))

;; Parser that reads bf-prog
;; bf-prog ::= (bf-op | bf-loop)*
(define bf-prog/p
  (bf-read/p (do
      [x <- (many/p (or/p bf-op/p bf-loop/p))]
      (pure `(bf-prog ,x)))))

(module+ test
  (require rackunit
           racket/string
           data/either)

  (define (check-bf-parsed? title prog)
    (check-pred success? (parse-string bf-prog/p prog) title))

  (check-bf-parsed? "Beautifulracket"
    "++++-+++-++-++[>++++-+++-++-++<-]>.")

  (check-bf-parsed? "Busy Beaver"
    ">+>+>+>+>+>+>+[->[>]+[->[>]+>+>+[<]+<]+<]+++++++[>+++++++++++>+<<-]>+.----.>++.")

  (check-bf-parsed? "Hello World" (string-join
    '("[]><[][]><[][]><[][]><[][]><[][]><[][]><[][]><[][]><[][]><[]"
      "[]>+>+>++>++<[>[->++++<<+++>]<<]>----.>->+.+++++++..+++.<+[]"
      "[ This is hellbox, a 104 command Hello World               ]"
      "[   >+>+>++>++<[>[->++++<<+++>]<<]>----.>>+.+++++++..+++   ]"
      "[   .>.<<<+++++++++++++++.>>.+++.------.--------.>+.>++.   ]"
      "[ -- Robert de Bath -- 2014                                ]"
      "[]>>.<<<+++++++++++++++.>>.+++.------.--------.>+.+>++.<<<[]"
      "[]><[][]><[][]><[][]><[][]><[][]><[][]><[][]><[][]><[][]><[]"
      ) "\n")))


;; Reader
;;
(require racket/port)

(provide read-syntax)

(define (read-syntax src-path in)
  (define src-string (port->string in))
  (define ast (parse-result! (parse-string (syntax/p bf-prog/p) src-string)))
  `(module bf-module "bf.rkt"
     ,ast))


;; Expander
;;
;; When a BrainFuck (bf) program starts, it creates an array of 30,000
;; cells in memory (with either 8/16/32 Bit cells) initialized to 0
;; and a pointer into that array (initialized to the 0 position). The
;; current byte is the byte in the array at the location indicated by
;; the pointer.
;;
;; Then it runs the code of the bf program, which consists of six
;; operations:
;; >  Increase the pointer position by one
;; <  Decrease the pointer position by one
;; +  Increase the value of the current byte by one
;; -  Decrease the value of the current byte by one
;; .  Write the current byte to stdout
;; , Read a byte from stdin and store it in the current byte
;;   (overwriting the existing value)
;;
;; bf also has a looping construct [...] that will repeat the code
;; within the brackets until the current byte is zero. If the current
;; byte is already zero, the loop will not run.
(require (for-syntax racket/base))

(provide (rename-out [bf-module-begin #%module-begin])
         #%top #%app #%datum #%top-interaction)

(define-syntax (bf-module-begin stx)
  (syntax-case stx ()
    [(_ BF-PROG)
     #'(#%module-begin
        ;; To debug, make it a symbol 'BF-PROG
        BF-PROG)]))

(define-syntax (bf-prog stx)
  (syntax-case stx ()
    [(_ (OP-OR-LOOP ...))
     ;; Executes instructions in sequence
     #'(begin OP-OR-LOOP ...)]))

(define-syntax (bf-loop stx)
  (syntax-case stx ()
    [(_ (OP-OR-LOOP ...))
     ;; Loop until current byte is zero
     #'(until (zero? (current-byte))
              OP-OR-LOOP ...)]))

(define-syntax (until stx)
  (syntax-case stx ()
    [(_ COND EXPR ...)
     #'(let loop ()
         (unless COND
           EXPR ...
           (loop)))]))

(define (bf-op op)
  (cond
    [(char=? op #\>) (set! ptr (add1 ptr))]
    [(char=? op #\<) (set! ptr (sub1 ptr))]
    [(char=? op #\+) (set-current-byte! (add1 (current-byte)))]
    [(char=? op #\-) (set-current-byte! (sub1 (current-byte)))]
    [(char=? op #\.) (write-byte (current-byte))]
    [(char=? op #\,) (set-current-byte! (read-byte))]
    [else (error 'bf-op "failed due to unrecognized op ~a" op)]))

(provide bf-prog bf-op bf-loop)

(define tape (make-vector 300000 0))
(define ptr 0)
(define (current-byte) (vector-ref tape ptr))
(define (set-current-byte! val) (vector-set! tape ptr val))
