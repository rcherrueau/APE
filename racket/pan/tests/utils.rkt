#lang typed/racket/base

(require (for-syntax racket/base
                     racket/path
                     racket/syntax
                     racket/string
                     )
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

         (only-in "../ast.rkt" Exp)
         (only-in "../asm.rkt" ASM)
         )

(require/typed racket/list
  ;; Coerce `flatten` to only return Listof String (instead of Listof
  ;; Any). This helps type-checking the `unlines` function.
  [flatten (Any -> (Listof String))])

(provide unlines
         tests-for-lang)


;; Haskell unlines. See,
;; https://hackage.haskell.org/package/base-4.11.0.0/docs/Prelude.html#v:unlines
(: unlines ((U String (Listof String))* -> String))
(define (unlines . words)
  (string-join (flatten words) (string #\newline) #:after-last (string #\newline)))


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
;; `lang-path`.
(: test-ast (String String String Exp -> Any))
(define (test-ast test-name lang-path prog-src expected-ast)
  (define prog-file
    (make-prog-file lang-path prog-src
                    (string-append "ast-" test-name "-" "~a")))

  (define ast
    (with-ouput-to-nowhere
      (dynamic-require prog-file 'ast)))

  (test-equal? test-name ast expected-ast))

;; Tests that program `prog-src` produced the `expected-asm` with lang
;; `lang-path`.
(: test-asm (String String String ASM -> Any))
(define (test-asm test-name lang-path prog-src expected-asm)
  (define prog-file
    (make-prog-file lang-path prog-src
                    (string-append "asm-" test-name "-" "~a")))

  (define asm
    (with-ouput-to-nowhere
      (dynamic-require prog-file 'asm)))

  (test-equal? test-name asm expected-asm))

;; Tests that program `prog-src` compiles to `expected-result` with
;; the lang `lang-path`.
(: test-compile (String String String String -> Any))
(define (test-compile test-name lang-path prog-src expected-result)
  (define prog-file
    (make-prog-file lang-path prog-src
                    (string-append "compile-" test-name "-" "~a")))

  (define result
    (with-output-to-string
      (λ () (dynamic-require prog-file #f))))

  (test-equal? test-name result expected-result))

;; Tests that the program `prog-src` fails with error message
;; `exn-predicate` during a compilation under `lang-path`.
(: test-not-compile (->* (String String String) ((U (Any -> Boolean) Regexp)) Any))
(define (test-not-compile test-name lang-path prog-src
                           [exn-predicate exn:fail:syntax?])
  (define prog-file
    (make-prog-file lang-path prog-src
                    (string-append "compile-" test-name "-" "~a")))

  (test-exn test-name exn-predicate (λ () (dynamic-require prog-file #f))))

;; Provide all previous tests for a specific `lang-path`.
(define (tests-for-lang [lang-path : String])
  (values
   (λ ([test-name : String] [prog-src : String] [ast : Exp])
     (test-ast test-name lang-path prog-src ast))
   (λ ([test-name : String] [prog-src : String] [asm : ASM])
     (test-asm test-name lang-path prog-src asm))
   (λ ([test-name : String] [prog-src : String] [out : String])
     (test-compile test-name lang-path prog-src out))
   (λ ([test-name : String] [prog-src : String] [ex : (U (Any -> Boolean) Regexp) exn:fail:syntax? ])
     (test-not-compile test-name lang-path prog-src ex))))
