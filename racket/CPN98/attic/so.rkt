;; See https://stackoverflow.com/q/60842192/2072144
#lang typed/racket/base

(module UNTYPED racket/base
  (require racket/set)
  (provide custom-set?
           make-immutable-custom-set)

  (define-custom-set-types custom-set
    #:elem? identifier?
    (λ (id1 id2) (eq? (syntax-e id1) (syntax-e id2))))

  (custom-set? #'(foo bar))
  )

(require/typed 'UNTYPED
  [#:opaque MySet custom-set?]
  [make-immutable-custom-set ((Listof Identifier) -> MySet)])

(custom-set? (make-immutable-custom-set (list #'foo #'bar)))  ;; #t
(custom-set? '())  ;; #f
(custom-set? #'(foo bar))
