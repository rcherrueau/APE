#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/define
                     racket/syntax
                     racket/match
                     )
         syntax/parse
         syntax/parse/define
         racket/match)

(provide (all-defined-out))


;; (define (check-classes-once stx)
;;   (define (get-class-name cls)
;;     (syntax-case cls (class)
;;       [(class name)          #'name]
;;       [(class name rest ...) #'name]
;;       [_ (raise-syntax-error #f "Bad syntax for class" cls)]))

;;   (let ([class-names (map get-class-name (syntax->list stx))])
;;     (check-duplicate-identifier class-names)))
