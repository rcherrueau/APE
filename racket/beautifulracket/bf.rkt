#lang racket/base

(require racket/port
         megaparsack megaparsack/text
         data/applicative data/monad
         (for-syntax racket/base)
         )

;; From http://beautifulracket.com


;; Parser
;;
;; bf-prog ::= (bf-op | bf-loop)*
;; bf-op   ::= '>' | '<' | '+' | '-' | '.' | ','
;; bf-loop ::= '[' bf-prog ']'

(define bf-op/p
  (do [x <- (char-in/p "><+-.,")]
      (pure `(bf-op ,x))))

(define bf-loop/p
  (do (char/p #\[)
      [x <- bf-prog/p]
      (char/p #\])
      (pure `(bf-loop ,x))))

(define bf-prog/p
  (do (many/p (or/p letter/p digit/p space/p))
      [x <- (many/p (or/p bf-op/p bf-loop/p))]
      (pure `(bf-prog ,x))))

(parse-string bf-prog/p "++++-+++-++-++[>++++-+++-++-++<-]>.
                         ++++-+++-++-++[>++++-+++-++-++<-]>.")


;; Reader
;;

;; (provide read-syntax)

;; (define (read-syntax src-path in)
;;
;;  )
