#lang typed/racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;;
;; Environments def for parsers of the lang.
;;
;; In transformation (e.g., desugar.rkt) I have to go with untyped
;; racket because it relies to much on `syntax-parse` and consœurs
;; which lack of good definitions in typed/racket.  But in general I
;; prefer typed code.  Typed code prevents to systematically tangle
;; annoying input-validation code in your function.
;;
;; In transformation, managing env should be done in typed/racket
;; because it only involve operation on list or hash
;; map. Unfortunately, I find it really hard to mix typed code with
;; untyped one... The `with-type` construction is not a good solution.
;; It doesn't benefit from type definitions outside of it (that I use
;; a lot with `typed/no-check` to document my code).  So I have to
;; write type annotation twice (outside `with-type` for documentation
;; and inside `with-type` for type checking).
;;
;; To circumvent this, and keep the code as clean as possible, I put
;; environments definition here.

(require racket/function
         racket/list
         racket/match
         syntax/parse
         typed/racket/unsafe
         "definitions.rkt"
         "utils.rkt"
         )

(require/typed "utils.rkt"
  ;; [check-stx=? (->* (Syntax Syntax) (String) Any)]
  [zip (All (a b) ((Listof a) (Listof b) -> (Listof (Pairof a b))))]
  [unzip (All (a b) ((Listof (Pairof a b)) -> (Values (Listof a) (Listof b))))]
  [bound-id=? (Identifier Identifier -> Boolean)])

(module+ test (require typed/rackunit))


;; General algebraic structures

;; A set of `elems` that uses the specific comparison function `eql?`.
;; This is something similar to immutable `define-custom-set-types`
;; but that works in the world of typed/racket
(struct [a] set ([elems : (Listof a)]
                 [eql? : (a a -> Boolean)])
  #:transparent
  #:constructor-name make-set
  #:type-name Setof)

;; Returns `#t` if `e` is in `st`, `#f` otherwise.
(: set-member? (All [a] ((Setof a) a -> Boolean)))
(define (set-member? st e)
  (let ([elems (set-elems st)]
        [eql?  (set-eql? st)])
    (and (findf (curry eql? e) elems) #t)))

;; Produces a set that includes `e` plus all elements of `st`.
(: set-add (All [a] ((Setof a) a -> (Setof a))))
(define (set-add st e)
  (let ([elems (set-elems st)]
        [eql?  (set-eql? st)])
    (if (set-member? st e) st
        (make-set (cons e elems) eql?))))



(module+ desugar
  (provide (all-defined-out))

  (define-predicate listof-ids? (Listof Identifier))

  ;; Make a new Γ
  (: make-Γ (() ((U (Listof Identifier)
                    (Syntaxof (Listof Identifier))
                    (Setof Identifier))) . ->* . (Setof Identifier)))
  (define (make-Γ [ids '()])
    (define (_make-Γ [_ids : (Listof Identifier)])
      (make-set _ids bound-id=?))

    (cond
      [(listof-ids? ids) (_make-Γ ids)]
      ;; Do nothing if its already a (Setof Identifier)
      [(set? ids) ids]
      [else (_make-Γ (syntax->list ids))]))

  ;; Is VAR bounded in Γ?
  (: Γ-member? ((Setof Identifier) Identifier -> Boolean))
  (define (Γ-member? Γ VAR) (set-member? Γ VAR))

  ;; Add a VAR to Γ
  (: Γ-add ((Setof Identifier) Identifier -> (Setof Identifier)))
  (define (Γ-add Γ VAR) (set-add Γ VAR))

  (module+ test
    (require typed/rackunit)
    (provide (all-defined-out))

    (define Γ-tests
      (test-suite
       "Test for Γ env"
       (for ([test:Γ (list (make-Γ (list #'foo #'bar))
                           (make-Γ #'(foo bar))
                           (make-Γ (make-Γ (list #'foo #'bar)))
                           (make-Γ (make-Γ #'(foo bar))))])
         (check-true  (Γ-member? test:Γ #'foo))
         (check-true  (Γ-member? test:Γ #'bar))
         (check-false (Γ-member? test:Γ #'baz))
         (check-true  (Γ-member? (Γ-add test:Γ #'baz) #'baz))
         (check-false (Γ-member? test:Γ #'baz)))))
    ))


;; basic-check

(module+ basic-check
  (provide (all-defined-out))

  ;; Make a new CS
  (: make-CS ((Listof B-TYPE) -> (Setof B-TYPE)))
  (define (make-CS ids) (make-set ids bound-id=?))

  ;; Is VAR bounded in CS?
  (: CS-member? ((Setof B-TYPE) B-TYPE -> Boolean))
  (define (CS-member? CS VAR) (set-member? CS VAR))

  (module+ test
    (require typed/rackunit)
    (provide (all-defined-out))
    (define test:CS (make-CS (list #'foo #'bar)))
    (define Γ-tests
      (test-suite
       "Test for Γ env"
       (check-true  (CS-member? test:CS #'foo))
       (check-true  (CS-member? test:CS #'bar))
       (check-false (CS-member? test:CS #'baz))))
    ))
