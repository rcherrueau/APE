#lang typed/racket/base/no-check

(require racket/dict)

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;;
;; (Untyped) common definitions for parsers of the lang.
;;
;; TODO: move all parts of this file into definitions.rkt

(require syntax/parse
         "utils.rkt")

(provide (all-defined-out))


;; TODO: "scheme" meaning? look at "Hindley-Milner Elaboration in
;; Application" paper, for a definition of scheme.
;;
;; http://gallium.inria.fr/~fpottier/publis/fpottier-elaboration.pdf
(define-syntax-class ow-scheme
  #:description "type with ownership and context parameters"
  #:attributes [OWNER CPARAMS TYPE]
  #:commit
  #:opaque
  (pattern (TYPE:id OWNER:id (CPARAM:id ...))
           #:with CPARAMS #'(CPARAM ...)))

;; (provide (struct-out immutable-custom-map))
;; (provide (struct-out IdIdMap) make-ididmap ididmap-ref ididmap-set ididmap-has-key?)

;; (define-type IdId (Syntaxof (Pairof Identifier Identifier)))

;; (define-custom-hash-types priv:idid-map
;;   #:key? (λ ([K : Idid])
;;            (if (syntax? K)
;;                (let ([k (syntax-e K)])
;;                  (and (pair? k) (identifier? (car k)) (identifier? (cdr k))))
;;                #f))
;;   (λ ([X : Idid] [Y : Idid])
;;     (let* ([x (syntax-e X)]
;;            [y (syntax-e Y)]
;;            [x1 (car x)] [x2 (cdr x)]
;;            [y1 (car y)] [y2 (cdr y)])
;;       (if (and (eq? (syntax-e x1) (syntax-e y1))
;;                (eq? (syntax-e x2) (syntax-e y2)))
;;           #t #f)))
;;   )

;; (struct IdIdMap ([dict : (Immutable-HashTable IdId Identifier)]))

;; (: make-ididmap
;;    (->* () ((Syntaxof (Listof (Pairof IdId Identifier)))) IdIdMap))
;; (define (make-ididmap [alist '()])
;;   (IdIdMap (make-immutable-priv:idid-map alist)))

;; (: ididmap-ref (IdIdMap IdId -> Identifier))
;; (define (ididmap-ref dict key)
;;   (dict-ref (IdIdMap-dict dict) key))

;; (: ididmap-set
;;    (IdIdMap IdId Identifier -> IdIdMap))
;; (define (ididmap-set dict key val)
;;   (IdIdMap (dict-set (IdIdMap-dict dict) key val)))

;; (: ididmap-has-key?
;;    (IdIdMap IdId -> Boolean))
;; (define (ididmap-has-key? dict key)
;;   (dict-has-key? (IdIdMap-dict dict) key))

;; #:methods gen:dict
;; [(define (dict-ref dict key [default (lambda () (error "key not found" key))])
;;    (dict-ref (IdidMap-dict dict) key default))
;;  (define (dict-set dict key val)
;;    (dict-set (IdidMap-dict dict) key val))
;;  (define (dict-remove dict key)
;;    (dict-remove (IdidMap-dict dict) key))
;;  (define (dict-iterate-first dict)
;;    (dict-iterate-first (IdidMap-dict dict)))
;;  (define (dict-iterate-next dict pos)
;;    (dict-iterate-next (IdidMap-dict dict) pos))
;;  (define (dict-iterate-key dict pos)
;;    (dict-iterate-key (IdidMap-dict dict) pos))
;;  (define (dict-iterate-value dict pos)
;;    (dict-iterate-value (IdidMap-dict dict) pos))
;;  (define (dict-map-key? dict key)
;;    (dict-map-key? (IdidMap-dict dict) key))
;;  (define (dict-count dict)
;;    (dict-count (IdidMap-dict dict)))
;;  ])
