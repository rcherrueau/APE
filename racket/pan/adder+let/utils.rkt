#lang typed/racket/base

(require (for-syntax racket/base
                     racket/path
                     syntax/parse
                     racket/syntax
                     racket/string)
         racket/file
         racket/function
         racket/port
         racket/provide-syntax
         racket/provide
         racket/require-syntax
         racket/string
         typed/rackunit
         typed/racket/unsafe

         (only-in "ast.rkt" Exp)
         )

(require/typed racket/list
  ;; Coerce `flatten` to only return Listof String (instead of Listof
  ;; Any). This helps type-checking the `unlines` function.
  [flatten (Any -> (Listof String))])

(provide unlines ∘ thunk
         primitive-op-id? unique-ids?
         ;; Rackunit
         test-ast test-asm test-compile
         test-not-compile)

;; Macros defined in typed modules may not be used in untyped modules.
;; A workaround for such macros is provided them with `unsafe-provide`
;; which exports the macro without any contracts generated. See,
;; https://docs.racket-lang.org/ts-guide/typed-untyped-interaction.html#%28part._.Using_.Typed_.Code_in_.Untyped_.Code%29
;; https://groups.google.com/d/msg/racket-users/eowl6RpdDwY/1wrCluDcAwAJ
(unsafe-provide snoc extends-lang)


;; Haskell unlines. See,
;; https://hackage.haskell.org/package/base-4.11.0.0/docs/Prelude.html#v:unlines
(: unlines ((U String (Listof String))* -> String))
(define (unlines . words)
  (string-join (flatten words) (string #\newline) #:after-last (string #\newline)))

;; Like `snoc`, but generalizes it to more than one list.
(define-syntax-rule (snoc AS ... A)
  (append AS ... (list A)))

;; Alias for compose1 (digraph C-k Ob)
(: ∘ (All (a b c) ((b → c) (a → b) → (a → c))))
(define ∘ compose1)

;; Check whether all identifier are unique, or not.
(: unique-ids? ((Syntaxof (Listof Identifier)) → Boolean))
(define (unique-ids? ids)
  (not (check-duplicate-identifier (syntax->list ids))))

;; Ensure the identifier refers to a primitive operation.
(: primitive-op-id? ((Syntaxof Any) → Boolean))
(define (primitive-op-id? id)
  (and (member (syntax->datum id) '(add1 sub1)) #t))


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


;; Rackunit utilites

;; Produces a temporary source program file with lang `lang.rkt` and
;; source `prog-src` and returns its location.
(: make-prog-file (String String -> Path))
(define (make-prog-file prog-src [template "rackettmp~a"])
  (define lang-abs-path (path->string (path->complete-path "lang.rkt")))
  (define prog-file (make-temporary-file template))

  (call-with-output-file prog-file #:exists 'replace
    (λ ([out : Output-Port])
      (displayln (format "#lang s-exp (file \"~a\")" lang-abs-path) out)
      (displayln prog-src out)))

  prog-file)

;; Flushes standard output to nowhere
(define-syntax-rule (with-ouput-to-nowhere PROC)
  (let ([out (open-output-nowhere)])
    (define result
      (parameterize ([current-output-port out])
        PROC))

    (unless (port-closed? out)
      (close-output-port out))

    result))

;; Tests that program `prog-src` produced the `expected-ast` with lang
;; `lang.rkt`.
(: test-ast (String String Exp -> Any))
(define (test-ast test-name prog-src expected-ast)
  (define prog-file
    (make-prog-file prog-src
                    (string-append "ast-" test-name "-" "~a")))

  (define ast
    (with-ouput-to-nowhere
      (dynamic-require prog-file 'ast)))

  (test-equal? test-name ast expected-ast))

;; Tests that program `prog-src` produced the `expected-asm` with lang
;; `lang.rkt`.
(: test-asm (String String Any #;ASM -> Any))
(define (test-asm test-name prog-src expected-asm)
  (define prog-file
    (make-prog-file prog-src
                    (string-append "asm-" test-name "-" "~a")))

  (define asm
    (with-ouput-to-nowhere
      (dynamic-require prog-file 'asm)))

  (test-equal? test-name asm expected-asm))

;; Tests that program `prog-src` compiles to `expected-result` with
;; the lang `lang.rkt`.
(: test-compile (String String String -> Any))
(define (test-compile test-name prog-src expected-result)
  (define prog-file
    (make-prog-file prog-src
                    (string-append "compile-" test-name "-" "~a")))

  (define result
    (with-output-to-string
      (λ () (dynamic-require prog-file #f))))

  (test-equal? test-name result expected-result))

;; Tests that the program `prog-src` fails with error message
;; `exn-predicate` during a compilation under `lang.rkt`.
(: test-not-compile (->* (String String) ((U (Any -> Boolean) Regexp)) Any))
(define (test-not-compile test-name prog-src
                           [exn-predicate exn:fail:syntax?])
  (define prog-file
    (make-prog-file prog-src
                    (string-append "compile-" test-name "-" "~a")))

  (test-exn test-name exn-predicate (λ () (dynamic-require prog-file #f))))
