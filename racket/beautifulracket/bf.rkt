#lang racket/base

;; From http://beautifulracket.com


;; Parser
;;
;; bf-prog ::= (bf-op | bf-loop)*
;; bf-op   ::= '>' | '<' | '+' | '-' | '.' | ','
;; bf-loop ::= '[' bf-prog ']'

(require megaparsack megaparsack/text
         data/applicative data/monad)

;; Util parser combinator that consumes chars that are not a tokens.
;; bf-read/p: parser → parser
(define (bf-read/p p)
  (define goto-next-token/p
    (many/p (or/p letter/p digit/p space/p)))

  (do goto-next-token/p
      [r <- p]
      goto-next-token/p
      (pure r)))

(define bf-op/p
  (bf-read/p (do
       [x <- (char-in/p "><+-.,")]
       (pure `(bf-op ,x)))))

(define bf-loop/p
  (bf-read/p (do
      (char/p #\[)
      [x <- (or/p (try/p bf-prog/p) (pure #f))]
      (char/p #\])
      (pure `(bf-loop ,x)))))

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

  (check-bf-parsed? "Beautifullracket"
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
  `(module bf-module racket
     ',ast))
