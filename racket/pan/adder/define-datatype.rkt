#lang typed/racket/base

;; Datatype
;; From: https://gist.github.com/lexi-lambda/18cf7a9156f743a1317e
;; See: https://lexi-lambda.github.io/blog/2015/12/21/adts-in-typed-racket-with-macros/

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     syntax/parse
                     syntax/stx)
         racket/match)

(provide define-datatype)

(begin-for-syntax
  (define-syntax-class type
    #:attributes [name [field-id 1] [param 1]]
    (pattern name:id
             #:attr [param 1] '()
             #:attr [field-id 1] '())
    (pattern (name:id param ...+)
             #:attr [field-id 1] (generate-temporaries #'(param ...)))))

(define-syntax define-datatype
  (syntax-parser
    [(_ type-name:type data-constructor:type ...)

     (define/with-syntax [data-type ...]
       (for/list ([name (in-syntax #'(data-constructor.name ...))])
         (if (stx-null? #'(type-name.param ...))
             name
             #`(#,name type-name.param ...))))

     #'(begin
         (struct (type-name.param ...) data-constructor.name
           ([data-constructor.field-id : data-constructor.param] ...) #:transparent) ...
         (define-type type-name (U data-type ...)))]))

#|

;; Usage

(define-datatype Expr
  (Value Number)
  (Add Expr Expr)
  (Subtract Expr Expr)
  (Multiply Expr Expr)
  (Divide Expr Expr))

(: evaluate (Expr -> Number))
(define (evaluate e)
  (match e
    [(Value x)      x                            ]
    [(Add a b)      (+ (evaluate a) (evaluate b))]
    [(Subtract a b) (- (evaluate a) (evaluate b))]
    [(Multiply a b) (* (evaluate a) (evaluate b))]
    [(Divide a b)   (/ (evaluate a) (evaluate b))]))

(evaluate (Add (Value 1)
               (Multiply (Divide (Value 1) (Value 2))
(Value 7))))

|#
