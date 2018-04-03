#lang typed/racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/path
                     racket/sequence
                     racket/string
                     racket/syntax
                     syntax/stx)
         racket/file
         racket/function
         racket/list
         racket/match
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
         test-compile
         test-not-compile
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


;; Rackunit utilities

;; Produces a temporary source program file with lang `lang-path` and
;; source `prog-src` and returns its location.
(: make-prog-file (String String String -> Path))
(define (make-prog-file lang-path prog-src [template "rackettmp~a"])
  (define lang-abs-path (path->string (path->complete-path lang-path)))
  (define prog-file (make-temporary-file template))

  (call-with-output-file prog-file #:exists 'replace
    (λ ([out : Output-Port])
      (displayln (format "#lang s-exp (file \"~a\")" lang-abs-path) out)
      (displayln prog-src out)))

  prog-file)

;; Tests that program `prog-src` compiles to `expected-result` with
;; the lang `lang-path`.
(: test-compile (String String String String -> Any))
(define (test-compile test-name lang-path prog-src expected-result)
  (define prog-file
    (make-prog-file lang-path prog-src (string-append test-name "-" "~a")))

  (define result
    (with-output-to-string
      (λ () (dynamic-require prog-file #f))))

  (test-equal? test-name result expected-result))

;; Tests that the program `prog-src` fails with error message
;; `exn-predicate` during a compilation under `lang-path`.
(: test-not-compile (String String String (U (Any -> Boolean) Regexp) -> Any))
(define (test-not-compile test-name lang-path prog-src
                           [exn-predicate exn:fail:syntax?])
  (define prog-file
    (make-prog-file lang-path prog-src (string-append test-name "-" "~a")))

  (test-exn test-name exn-predicate (λ () (dynamic-require prog-file #f))))


;; The extends-lang macro: reuse all from a `base-lang` except
;; `compile-exp`. Imports functions from `base-lang` with prefix
;; "base-lang:". Provides functions from `base-lang` without prefix.
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
  (syntax-case stx ()
    [(_ BASE-LANG)
     (let* ([str-pre (extract-file-name (syntax->datum #'BASE-LANG))]
            [str-pre: (format "~a:" str-pre)]
            [pre: (format-id #'BASE-LANG "~a:" str-pre)]
            [pre:c-exp (format-id #'BASE-LANG "~a:compile-exp" str-pre)])
       #`(filtered-out
          ;; Remove prefix from the name
          (λ (name) (string-replace name #,str-pre: ""))
          ;; Also remove `#%module-begin`
          (except-out (all-from-out BASE-LANG)
                      #,pre:c-exp)))]))

(define-syntax-rule (extends-lang BASE-LANG)
  (begin
    (require (require-lang BASE-LANG))
    (provide (provide-lang BASE-LANG))))


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
           ([data-constructor.field-id : data-constructor.param] ...)) ...
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
