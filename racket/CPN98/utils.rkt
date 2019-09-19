#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/define
                     racket/syntax
                     racket/match
                     )
         syntax/parse
         syntax/parse/define
         racket/match
         racket/list)

(provide (all-defined-out))



;; -- Syntax checker in the form of
;; https://docs.racket-lang.org/syntax/syntax-helpers.html#%28part._stxkeyword%29
;; A check procedure consumes the syntax to check and a context
;; syntax object for error reporting and either raises an error to
;; reject the syntax or returns a value as its parsed
;; representation.

;; Returns the first duplicate class in the program or #f if there
;; are no duplicate.
(define (check-class clss-stx)
  (define (get-class-name cls-stx)
    (syntax-case cls-stx (class)
      [(class name)       #'name]
      [(class name _ ...) #'name]
      [_ (raise-syntax-error #f "Bad syntax for class" cls-stx)]))

  (let* ([classes     (syntax->list clss-stx)]
         [class-names (map get-class-name classes)])
    (check-duplicate-identifier class-names)))

;; Returns the first duplicate field in the class or #f if there are
;; no duplicate.
(define (field-twice? cls-stx)
  (define (get-field-name field/def-stx)
    (syntax-case field/def-stx (field :)
      ;; (field [NAME : TYPE])
      [(field [name : _]) #'name]
      [_                  #f]))

  (let* ([fields/defs (syntax->list cls-stx)]
         [field-names (filter-map get-field-name fields/defs)])
    (check-duplicate-identifier field-names)))

;; Returns the first duplicate def in the class or #f if there are
;; no duplicate.
(define (def-twice? cls-stx)
  (define (get-def-name field/def-stx)
    (syntax-case field/def-stx (def : →)
      ;; (def (NAME [ARG:NAME : ARG:TYPE] ... → RET:TYPE) E)
      [(def (name _ ...) _) #'name]
      [_                      #f]))

  (let* ([fields/defs (syntax->list cls-stx)]
         [def-names   (filter-map get-def-name fields/defs)])
    (check-duplicate-identifier def-names)))
