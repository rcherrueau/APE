#lang racket/base
(require (for-syntax racket/base
                     racket/string
                     racket/path
                     racket/syntax
                     )
         racket/require-syntax
         racket/provide-syntax
         racket/provide
         )

(provide (all-defined-out))

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
            [pre:mb (format-id #'BASE-LANG "~a:#%module-begin" str-pre)])
       #`(filtered-out
          ;; Remove prefix from the name
          (λ (name) (string-replace name #,str-pre: ""))
          ;; Also remove `#%module-begin`
          (except-out (all-from-out BASE-LANG)
                      #,pre:mb)))]))
(define-syntax-rule (extends-lang BASE-LANG)
  (begin
    (require (require-lang BASE-LANG))
    (provide (provide-lang BASE-LANG))))


