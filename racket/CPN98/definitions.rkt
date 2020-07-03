#lang typed/racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;;
;; Common definitions for parsers of the lang.

(require (for-syntax racket/base)
         racket/function
         racket/list
         racket/match
         syntax/parse
         typed/racket/unsafe
         "_definitions.rkt"
         "utils.rkt")

(module+ test (require typed/rackunit))

(require/typed "utils.rkt"
  [check-stx=? ((Syntax Syntax) (#:msg String) . ->* . Any)]
  [zip (All (a b) ((Listof a) (Listof b) -> (Listof (Pairof a b))))]
  [unzip (All (a b) ((Listof (Pairof a b)) -> (Values (Listof a) (Listof b))))])

(require/typed racket/list
  [index-of (All (a) ((Listof a) a (a a -> Boolean) -> (U Nonnegative-Integer #f)))])

(unsafe-require/typed racket/sequence
  [in-syntax (All (a) (Syntaxof (Listof (Syntaxof a))) -> (Sequenceof (Syntaxof a)))])

(unsafe-require/typed "utils.rkt"
  [mk-ow-type-surface (TYPE OWNER CPARAMS -> Syntax)]
  [set-surface-stx (All (a) (Syntaxof a) Syntax -> (Syntaxof a))]
  [get-surface-stx (Syntax -> Syntax)])

(provide (except-out (all-defined-out)
                     ;; keyword-lits expr-lits
                     mk-ow-type
                     def-ow-type-values
                     private:alist-index-of
                     alist-set)
         (all-from-out "_definitions.rkt"))

(unsafe-provide mk-ow-type def-ow-type-values)


;; Utils
(: bound-id=? (Identifier Identifier -> Boolean))
(define (bound-id=? id1 id2)
  (eq? (syntax-e id1) (syntax-e id2)))


;; Ownership type syntax object

;; Definitions
(define-type TYPE Identifier)                                 ;; Basic type #'Int, #'Bool, ...
(define-type OWNER Identifier)                                ;; Owner #'rep, #'world, #'Θ, #'o, ...
(define-type CPARAMS (Syntaxof (Listof Identifier)))         ;; List of ctx params #'(), #'(rep n)
(define-type OW-TYPE (Syntaxof (List TYPE OWNER CPARAMS))) ;; Ownership type #'(Foo rep {n m})

;; Predicate for occurrence typing, see [LLNC19] and
;; https://docs.racket-lang.org/ts-guide/occurrence-typing.html
(define-predicate type? TYPE)
(define-predicate ow-type? OW-TYPE)

;; Make an ownership type
(: mk-ow-type
   ((TYPE OWNER (U CPARAMS (Listof Identifier)))
    (#:surface Syntax) . ->* . OW-TYPE))
(define (mk-ow-type type-stx owner-stx cparams #:surface [surface #f])
  ;; Make cparams-stx of type CPARAMS and not a `(Listof Identifier)`
  (define cparams-stx : CPARAMS
    (cond
      ;; This is the empty list
      [(and (list? cparams) (null? cparams) #'())]
      ;; This is a non empty list
      [(list? cparams)
       (let ([ctx0 (car cparams)])
         (cast (datum->syntax ctx0 cparams ctx0) CPARAMS))]
      ;; This is already a CPARAMS
      [else cparams]))

  ;; Get the surface stx of this type, i.e., owner/type{cparams ...}
  (define surface-stx
    (or surface (mk-ow-type-surface type-stx owner-stx cparams-stx)))

  ;; Make the ownership type
  (define ow-type : OW-TYPE
    (cast (datum->syntax surface-stx   ; ctx
                         `(,type-stx ,owner-stx ,cparams-stx)
                         surface-stx   ; srcloc
                         surface-stx)  ; prop
          OW-TYPE))

  ;; Mark its surface
  ((inst set-surface-stx (List TYPE OWNER CPARAMS)) ow-type surface-stx))

;; Get ownership type values
(: ow-type-values (OW-TYPE -> (List TYPE OWNER (Listof Identifier))))
(define (ow-type-values ow-type-stx)
  (match-let ([(list ot.type ot.owner ot.cparams) (syntax-e ow-type-stx)])
    (list ot.type ot.owner (syntax->list ot.cparams))))

;; Destructs an ownership type and binds its values to identifiers
;;
;; > (def-ow-type-values (the-type the-owner the-cparams) #'(Foo o {n m}))
;; > (printf "type: ~a, owner: ~a, params: ~a" the-type the-owner the-cparams)
(define-syntax-rule (def-ow-type-values (type-stx owner-stx cparams-stx) E)
  (match-define (list type-stx owner-stx cparams-stx)
    (ow-type-values E)))

(module+ test
  (check-true  (type? #'Foo))
  (check-true  (type? #'o))
  (check-false (type? #'1))

  (check-true  (ow-type? #'(Foo o {})))
  (check-true  (ow-type? #'(Foo o {n m})))
  (check-false (ow-type? #'(Foo 1 {n m})))

  (check-true (ow-type? (mk-ow-type #'Foo #'o #'{n m})))
  (check-true (ow-type? (mk-ow-type #'Foo #'o {list #'n #'m})))

  (check-stx=? (get-surface-stx (mk-ow-type #'Foo #'o #'{n m})) #'|o/Foo{n m}|)
  (check-stx=? (get-surface-stx (mk-ow-type #'Foo #'o #'{n m} #:surface #'a)) #'a))



;; Get or set the basic type of a syntax object.
(: b-type-prop
   (All (a) (case->
             [(Syntaxof a) -> (U TYPE #f)]
             [(Syntaxof a) TYPE -> (Syntaxof a)])))
(define b-type-prop
  (case-lambda
    ;; Get the b-type of a syntax object.
    [(stx) (let ([τ (syntax-property stx 'b-type)])
             (and (type? τ) τ))]
    ;; Set the basic type of a syntax object
    [(stx TYPE) (syntax-property stx 'b-type TYPE)]))

;; Get or set the ownership type of a syntax object.
(: ow-type-prop
   (All (a) (case->
             [(Syntaxof a) -> (U OW-TYPE #f)]
             [(Syntaxof a) OW-TYPE -> (Syntaxof a)])))
(define ow-type-prop
  (case-lambda
    ;; Get the b-type of a syntax object.
    [(stx) (let ([τ (syntax-property stx 'ow-type)])
             (and (ow-type? τ) τ))]
    ;; Set the basic type of a syntax object
    [(stx TYPE) (syntax-property stx 'ow-type TYPE)]))


;; Meta

;; An associative list with custom function for comparing keys.
(define-type (AList k v) (Listof (Pairof k v)))
(define-type (~> k v) (AList k v))

;; Returns the length of `als`
(: alist-length (All (k v) ((AList k v) -> Index)))
(define (alist-length als)
  (length als))

;; Returns the value for `key` in `als` using `key-eq?` comparison
(: alist-ref (All (k v) (((AList k v) k (k k -> Boolean)) ((U (k -> v) #f)) . ->* . v)))
(define (alist-ref als key key-eq? [failure-result #f])
  (cond
    [(assoc key als key-eq?) => cdr]
    [else (if failure-result (failure-result key)
              (error "the key ~s does not exist in AList" key))]))

;; Functionally extends `als` by mapping `key` to `val`,
;; overwriting any existing mapping for `key`, and returning an
;; extended dict.
(: alist-set (All (k v) ((AList k v) k v (k k -> Boolean) -> (AList k v))))
(define (alist-set als key val key-eq?)
  (cond
    [(private:alist-index-of als key key-eq?)
     => (λ (idx) (list-set als idx (cons key val)))]
    [else (cons (cons key val) als)]))

;; Indice of the `key` in `als`
(: private:alist-index-of
   (All (k v) ((AList k v) k (k k -> Boolean) -> (U Nonnegative-Integer #f))))
(define (private:alist-index-of als key key-eq?)
  (let-values ([(keys _) (unzip als)])
    (index-of keys key key-eq?)))

;; Returns `#t` if `als` contains a value for the given `key`,
;; `#f` otherwise.
(: alist-has-key? (All (k v) ((AList k v) k (k k -> Boolean) -> Boolean)))
(define (alist-has-key? als key key-eq?)
  (and (private:alist-index-of als key key-eq?) #t))

;; Returns `#t` if `als1` is equal to `als2` according to `k-eq?` to
;; compare keys and `v-eq?` to compare values.
(: alist-eq?
   (All (k v) ((AList k v) (AList k v) (k k -> Boolean) (v v -> Boolean) -> Boolean)))
(define (alist-eq? als1 als2 k-eq? v-eq?)
  (cond
    ;; Both dict should contain the same number of entries
    [(not (eq? (alist-length als1) (alist-length als2))) #f]
    [else
     (for/and ([kv als1])
       (let ([k (car kv)]
             [v (cdr kv)])
         (and
          ;; k exists in als2
          (alist-has-key? als2 k k-eq?)
          ;; value of als1(k) and als2(k) are equal
          (v-eq? v (alist-ref als2 k k-eq?)))))]))

;; Maps values of `als` with `f`
(: alist-map (All (k v w) ((v -> w) (AList k v) -> (AList k w))))
(define (alist-map f als)
  (map (λ ([kv : (Pairof k v)])
         (let ([k (car kv)]
               [v (cdr kv)])
           (cons k (f v))))
       als))

;; Maps values of `als` with `f`
(: meta-map (All (k v w) ((v -> w) (AList k v) -> (AList k w))))
(define meta-map alist-map)

;; Maps keys of `als` with `f`
(: alist-kmap (All (k l v) ((k -> l) (AList k v) -> (AList l v))))
(define (alist-kmap f als)
  (map (λ ([kv : (Pairof k v)])
         (let ([k (car kv)]
               [v (cdr kv)])
           (cons (f k) v)))
       als))

;; Maps keys of `als` with `f`
(: meta-kmap (All (k l v) ((k -> l) (AList k v) -> (AList l v))))
(define meta-kmap alist-kmap)

(: alist-map-w/key (All (k l v w) (((Pairof k v) -> (Pairof l w)) (AList k v) -> (AList l w))))
(define (alist-map-w/key f als)
  (map f als))

(: meta-map-w/key (All (k l v w) (((Pairof k v) -> (Pairof l w)) (AList k v) -> (AList l w))))
(define meta-map-w/key alist-map-w/key)

;; Returns the keys of `als`
(: alist-keys (All (k v) (AList k v) -> (Listof k)))
(define (alist-keys als)
  (map (λ ([kv : (Pairof k v)]) (car kv)) als))

;;~~~~~~~~~~~~~~~~~
;; CS: Set of Types
(define-type CS (Listof Identifier))

;;~~~~~~~~~~~~~~~~~~
;; FS: Map of fields
;;
;; With the syntax #'(Class-type . Field-name) as key and the Field
;; OW-TYPE as value.
(define-type FS-key (Syntaxof (Pairof TYPE Identifier)))
(define-type FS (AList FS-key OW-TYPE))

(module+ test

  ;; Test two FS-key are equals
  ;; (fs-key=? #'(a . b) #'(a . b))  ; #t
  ;; (fs-key=? #'(a . b) #'(c . d))  ; #f
  ;; (fs-key=? #'(a . 1) #'(a . 1))  ; #f
  (: fs-key=? (FS-key FS-key -> Boolean))
  (define (fs-key=? key1-stx key2-stx)
    (match-let ([(cons C-TYPE1 F-NAME1) (syntax-e key1-stx)]
                [(cons C-TYPE2 F-NAME2) (syntax-e key2-stx)])
      (and
       (bound-id=? C-TYPE1 C-TYPE2)
       (bound-id=? F-NAME1 F-NAME2))))

  ;; Test FS with `fs-key=?`
  ;; (define-values (FS-ref FS-set FS-member? FS-eq?) (make-FS fs-key=?))
  ;; (define test:τFoo #'(foo-type rep ()))
  ;; (define test:τBar #'(bar-type rep ()))

  ;; (: test:FS FS)
  ;; (define test:FS
  ;;   `((,#'(c . foo) . ,test:τFoo)
  ;;     (,#'(c . bar) . ,test:τBar)))

  ;; (: stx-eq? (OW-TYPE OW-TYPE -> Boolean))
  ;; (define (stx-eq? a b)
  ;;   (equal? (syntax->datum a) (syntax->datum b)))

  ;; (parameterize ([meta:FS test:FS])
  ;;   (check-true  (FS-member? #'(c . foo)))
  ;;   (check-false (FS-member? #'(C . foo)))
  ;;   (check-false (FS-member? #'(c . Foo)))
  ;;   (check-false (FS-member? #'(c . baz)))

  ;;   (check-stx=? (FS-ref #'(c . foo)) test:τFoo)
  ;;   (check-stx=? (FS-ref #'(c . bar)) test:τBar)

  ;;   (check-true  (FS-eq? (FS-set #'(c . foo) test:τFoo) stx-eq?))
  ;;   (check-false (FS-eq? (FS-set #'(C . foo) test:τFoo) stx-eq?))
  ;;   (check-false (FS-eq? (FS-set #'(c . Foo) test:τFoo) stx-eq?))
  ;;   (check-false (FS-eq? (FS-set #'(c . foo) test:τBar) stx-eq?))
  ;;   (check-false (FS-eq? (FS-set #'(c . baz) test:τBar) stx-eq?)))
  )

;;~~~~~~~~~~~~~~~~~~~~~~~
;; DS: Map of definitions
;;
;; With the syntax #'(Class-type Def-name (Def-arg-type ...)) as key
;; and the Def return type as value.
(define-type DS-key (Syntaxof
                     (List Identifier                     ; Class type
                           Identifier                     ; Def name
                           (Syntaxof (Listof OW-TYPE))  ; Type of def args
                           )))

(define-type DS (AList DS-key OW-TYPE))
;; (: meta:DS (Boxof DS))
;; (define meta:DS (box '()))

;; (define (ow-type=?))
;; (ds-key=? #'(a b ()) #'(a b ()))                  ; #t
;; (ds-key=? #'(a b [(c d ())]) #'(a b [(c d ())]))  ; #t
;; (ds-key=? #'(a b ()) #'(c d ()))                  ; #f
;; (ds-key=? #'(a b [(c d ())]) #'(a b [(d c ())]))  ; #f
(: ds-key=? ((OW-TYPE OW-TYPE -> Boolean) DS-key DS-key -> Boolean))
(define (ds-key=? ow-type=? key1-stx key2-stx)
  (match-let ([(list C-TYPE1 D-NAME1 ARGs-OW-TYPE1) (syntax-e key1-stx)]
              [(list C-TYPE2 D-NAME2 ARGs-OW-TYPE2) (syntax-e key2-stx)])
    (and
     (bound-id=? C-TYPE1 C-TYPE2)
     (bound-id=? D-NAME1 D-NAME2)
     (for/and ([ow1 (in-syntax ARGs-OW-TYPE1)]
               [ow2 (in-syntax ARGs-OW-TYPE2)])
       (ow-type=? ow1 ow2)))))


;; Bibliography
;;
;; @article{LLNC19,
;;   author    = {Victor Lanvin and Micka{\"{e}}l Laurent and
;;                Kim Nguyen and Giuseppe Castagna},
;;   title     = {Revisiting Occurrence Typing},
;;   journal   = {CoRR},
;;   volume    = {abs/1907.05590},
;;   year      = {2019},
;;   url       = {http://arxiv.org/abs/1907.05590},
;;   archivePrefix = {arXiv},
;;   eprint    = {1907.05590},
;;   timestamp = {Wed, 17 Jul 2019 10:27:36 +0200},
;; }
