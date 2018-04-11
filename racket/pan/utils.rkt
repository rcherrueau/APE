#lang typed/racket/base

(require (for-syntax racket/base
                     racket/sequence
                     racket/path
                     syntax/parse
                     racket/syntax
                     racket/string
                     syntax/stx)
         racket/file
         racket/function
         racket/path
         racket/port
         racket/provide-syntax
         racket/provide
         racket/require-syntax
         racket/string
         typed/rackunit
         typed/racket/unsafe
         )

(require/typed racket/list
  ;; Coerce `flatten` to only return Listof String (instead of Listof
  ;; Any). This helps type-checking the `unlines` function.
  [flatten (Any -> (Listof String))])

(provide unlines
         snoc
         define-datatype)

;; Macros defined in typed modules may not be used in untyped modules.
;; A workaround for such macros is provided them with `unsafe-provide`
;; which exports the macro without any contracts generated. See,
;; https://groups.google.com/d/msg/racket-users/eowl6RpdDwY/1wrCluDcAwAJ
(unsafe-provide extends-lang)


;; Haskell unlines. See,
;; https://hackage.haskell.org/package/base-4.11.0.0/docs/Prelude.html#v:unlines
(: unlines ((U String (Listof String))* -> String))
(define (unlines . words)
  (string-join (flatten words) (string #\newline) #:after-last (string #\newline)))

;; Inverse of `cons`. Appends a `A` at the end of a `Listof A`.
(: snoc (All (A) ((Listof A) A -> (Listof A))))
(define (snoc as a)
  (append as (list a)))


;; The extends-lang macro: reuse all from a `base-lang` except
;; `compile-exp` and excluded macro. Imports functions from
;; `base-lang` with prefix "base-lang:". Provides functions from
;; `base-lang` without prefix.
(define-for-syntax (extract-file-name file)
  (define f (if (string? file) file (symbol->string file)))
  (path->string (path-replace-suffix (file-name-from-path f) "")))

(define-require-syntax (require-lang stx)
  (syntax-case stx ()
    [(_ BASE-LANG)
     (let* ([pre (extract-file-name (syntax->datum #'BASE-LANG))]
            [pre: (format-id #'BASE-LANG "~a:" pre)])
       ;; Import with prefix to avoid collusion
       #`(prefix-in #,pre: BASE-LANG))]))

(define-provide-syntax (provide-lang stx)
  (syntax-parse stx
    [(_ BASE-LANG (~optional (~seq EXCLUDE ...)))
     (let* ([lang-pre (extract-file-name (syntax->datum #'BASE-LANG))]
            [lang-pre: (format "~a:" lang-pre)]
            [make-pre:id (λ (id) (format-id #'BASE-LANG "~a:~a" lang-pre id))]
            [pre:excludes (map make-pre:id
                               (cons "compile-exp"
                                     (syntax->list #'(EXCLUDE ...))))])
       #`(filtered-out
           ;; Remove prefix from the name
           (λ (name) (string-replace name #,lang-pre: ""))
           ;; Also remove `compile-exp` and EXCLUDE ...
           (except-out (all-from-out BASE-LANG)
                       #,@pre:excludes)))]))

(define-syntax (extends-lang stx)
  (syntax-parse stx
    [(_ BASE-LANG)
     #'(begin
         (require (require-lang BASE-LANG))
         (provide (provide-lang BASE-LANG)))]
    [(_ BASE-LANG #:override MACRO-NAME ...)
     #'(begin
         (require (require-lang BASE-LANG))
         (provide (provide-lang BASE-LANG MACRO-NAME ...)))]))


;; Datatype
;; From: https://gist.github.com/lexi-lambda/18cf7a9156f743a1317e
;; See: https://lexi-lambda.github.io/blog/2015/12/21/adts-in-typed-racket-with-macros/

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
