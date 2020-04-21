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
         "utils.rkt"
         )

(module+ test (require typed/rackunit))

;; (require/typed syntax/parse
;;   ;; Coerce `flatten` to only return Listof String (instead of Listof
;;   ;; Any). This helps type-checking the `unlines` function.
;;   [syntax-parser (Any -> Any)])
(require/typed "utils.rkt"
  ;; [check-stx=? (->* (Syntax Syntax) (String) Any)]
  [zip (All (a b) ((Listof a) (Listof b) -> (Listof (Pairof a b))))]
  [unzip (All (a b) ((Listof (Pairof a b)) -> (Values (Listof a) (Listof b))))]
  [bound-id=? (Identifier Identifier -> Boolean)])

(require/typed racket/list
  [index-of (All (a) ((Listof a) a (a a -> Boolean) -> (U Nonnegative-Integer #f)))])

(provide (except-out (all-defined-out)
                     ;; keyword-lits expr-lits
                     private:dict-index-of
                     dict-set
                     ;; FIXME: make them private and define a proper
                     ;; interface to define them in order to ensure
                     ;; type safety with contracts.
                     ;; meta:FS
                     )
         (all-from-out "_definitions.rkt")
         )

;; Provide without the generation of contracts
;;
;; Macros defined in typed modules may not be used in untyped modules.
;; A workaround for such macros is provided them with `unsafe-provide`
;; which exports the macro without any contracts generated. See,
;; https://docs.racket-lang.org/ts-guide/typed-untyped-interaction.html#%28part._.Using_.Typed_.Code_in_.Untyped_.Code%29
;; https://groups.google.com/d/msg/racket-users/eowl6RpdDwY/1wrCluDcAwAJ
(unsafe-provide ;; keyword-lits expr-lits
                #;meta:FS)

;; TODO: Use https://docs.racket-lang.org/ts-reference/Utilities.html#%28part._.Untyped_.Utilities%29
;; for a better def of typed dict


;; Utils

;; (: bound-id=? (Identifier Identifier -> Boolean))
;; (define (bound-id=? id1 id2)
;;   (eq? (syntax-e id1) (syntax-e id2)))

;; A dict with custom function for comparing keys.
(define-type (Dict k v) (Listof (Pairof k v)))

;; Returns the length of `the-dict`
(: dict-length (All (k v) ((Dict k v) -> Index)))
(define (dict-length the-dict)
  (length the-dict))

;; Returns the value for `key` in `the-dict` using `key-eq?` comparison
(: dict-ref (All (k v) ((Dict k v) k (k k -> Boolean) -> v)))
(define (dict-ref the-dict key key-eq?)
  (cond
    [(assoc key the-dict key-eq?) => cdr]
    [else (error "the key ~s does not exist in Dict" key)]))

;; Functionally extends `the-dict` by mapping `key` to `val`,
;; overwriting any existing mapping for `key`, and returning an
;; extended dict.
(: dict-set (All (k v) ((Dict k v) k v (k k -> Boolean) -> (Dict k v))))
(define (dict-set the-dict key val key-eq?)
  (cond
    [(private:dict-index-of the-dict key key-eq?)
     => (λ (idx) (list-set the-dict idx (cons key val)))]
    [else (cons (cons key val) the-dict)]))

;; Indice of the `key` in `the-dict`
(: private:dict-index-of
   (All (k v) ((Dict k v) k (k k -> Boolean) -> (U Nonnegative-Integer #f))))
(define (private:dict-index-of the-dict key key-eq?)
  (let-values ([(keys _) (unzip the-dict)])
    (index-of keys key key-eq?)))

;; Returns `#t` if `the-dict` contains a value for the given `key`,
;; `#f` otherwise.
(: dict-has-key? (All (k v) ((Dict k v) k (k k -> Boolean) -> Boolean)))
(define (dict-has-key? the-dict key key-eq?)
  (and (private:dict-index-of the-dict key key-eq?) #t))

;; Returns `#t` if `dict1` is equal to `dict2` according to `k-eq?` to
;; compare keys and `v-eq?` to compare values.
(: dict-eq?
   (All (k v) ((Dict k v) (Dict k v) (k k -> Boolean) (v v -> Boolean) -> Boolean)))
(define (dict-eq? dict1 dict2 k-eq? v-eq?)
  (cond
    ;; Both dict should contain the same number of entries
    [(not (eq? (dict-length dict1) (dict-length dict2))) #f]
    [else
     (for/and ([kv dict1])
       (let ([k (car kv)]
             [v (cdr kv)])
         (and
          ;; k exists in dict2
          (dict-has-key? dict2 k k-eq?)
          ;; value of dict1(k) and dict2(k) are equal
          (v-eq? v (dict-ref dict2 k k-eq?)))))]))

;; Maps values of `the-dict` with `f`
(: dict-map (All (k v w) ((v -> w) (Dict k v) -> (Dict k w))))
(define (dict-map f the-dict)
  (map (λ ([kv : (Pairof k v)])
         (let ([k (car kv)]
               [v (cdr kv)])
           (cons k (f v))))
       the-dict))

;; Maps values of `the-dict` with `f`
(: meta-map (All (k v w) ((v -> w) (Dict k v) -> (Dict k w))))
(define (meta-map f the-dict)
  (map (λ ([kv : (Pairof k v)])
         (let ([k (car kv)]
               [v (cdr kv)])
           (cons k (f v))))
       the-dict))

;; Maps keys of `the-dict` with `f`
(: dict-kmap (All (k l v) ((k -> l) (Dict k v) -> (Dict l v))))
(define (dict-kmap f the-dict)
  (map (λ ([kv : (Pairof k v)])
         (let ([k (car kv)]
               [v (cdr kv)])
           (cons (f k) v)))
       the-dict))

;; Maps keys of `the-dict` with `f`
(: meta-kmap (All (k l v) ((k -> l) (Dict k v) -> (Dict l v))))
(define (meta-kmap f the-dict)
  (map (λ ([kv : (Pairof k v)])
         (let ([k (car kv)]
               [v (cdr kv)])
           (cons (f k) v)))
       the-dict))

(: meta-map-kv (All (k l v w) ((k -> l) (v -> w) (Dict k v) -> (Dict l w))))
(define (meta-map-kv f g the-dict)
  (map (λ ([kv : (Pairof k v)])
         (let ([k (car kv)]
               [v (cdr kv)])
           (cons (f k) (g v))))
       the-dict))

;; Returns the keys of `the-dict`
(: dict-keys (All (k v) (Dict k v) -> (Listof k)))
(define (dict-keys the-dict)
  (map (λ ([kv : (Pairof k v)]) (car kv)) the-dict))

;; Make a dict
(: make-dict (All (a b) ((Parameterof (Dict a b))
                        (a a -> Boolean) ->
                        (Values
                         ;; dict-ref
                         (a -> b)
                         ;; dict-set
                         (a b -> (Dict a b))
                         ;; dict-has-key?
                         (a -> Boolean)
                         ;; dict-eq?
                         ((Dict a b) (b b -> Boolean) -> Boolean)))))
(define (make-dict the-dict k-eq?)
  (values
   (λ (key) (dict-ref (the-dict) key k-eq?))
   (λ (key val) (dict-set (the-dict) key val k-eq?))
   (λ (key) (dict-has-key? (the-dict) key k-eq?))
   (λ (dict2 v-eq?) (dict-eq? (the-dict) dict2 k-eq? v-eq?))))


;; Language definitions

(define-type B-TYPE Identifier)                        ;; Basic Type
(define-type OWNER Identifier)                         ;; Owner
(define-type C-PARAMS (Syntaxof (Listof Identifier)))  ;; List of ctx params

;; Ownership scheme
;; #'(a b ())
;; #'(a b [c d])
(define-type OW-SCHEME (Syntaxof (List B-TYPE OWNER C-PARAMS)))

;; > (ow-scheme? #'(a b ()))       ; #t
;; > (ow-scheme? #'(a b [c d e]))  ; #t
;; > (ow-scheme? #'(a 1 []))       ; #f
(define-predicate ow-scheme? OW-SCHEME)
(define-predicate b-type? B-TYPE)
(define-predicate owner? OWNER)
(define-predicate c-params? C-PARAMS)

(: ow-scheme->b-type   (OW-SCHEME -> B-TYPE))
(: ow-scheme->owner    (OW-SCHEME -> Identifier))
(: ow-scheme->c-params (OW-SCHEME -> (Syntaxof (Listof Identifier))))
(define (ow-scheme->b-type ows) (car (syntax-e ows)))
(define (ow-scheme->owner ows) (cadr (syntax-e ows)))
(define (ow-scheme->c-params ows) (caddr (syntax-e ows)))

;; Cast a B-TYPE into a degenerated OW-SCHEME
(: b-type->ow-scheme (B-TYPE -> OW-SCHEME))
(define (b-type->ow-scheme B-TYPE)
  (define b-type (syntax-e B-TYPE))
  (cast (datum->syntax B-TYPE `(,b-type ⊥ ()) B-TYPE)
        OW-SCHEME))

;; Compare two ow-scheme, but does not instantiate the owner and
;; context parameters. So it only ensures that both basic types are
;; bound and both contains the same number of context parameters.
;;
;; > (type=? #'(a b ()) #'(a b ()))            ; #t
;; > (type=? #'(a b [c d e]) #'(a b [c d e]))  ; #t
;; > (type=? #'(a b [c d e]) #'(a b [e d c]))  ; #t
;; > (type=? #'(a b [c d e]) #'(a z [c d e]))  ; #t
;; > (type=? #'(a b ()) #'(c b ()))            ; #f
(: type=? (OW-SCHEME OW-SCHEME -> Boolean))
(define (type=? ow1 ow2)
  (match-let ([(list TYPE1 OWNER1 CPARAMS1) (syntax-e ow1)]
              [(list TYPE2 OWNER2 CPARAMS2) (syntax-e ow2)])
    (and (bound-id=? TYPE1 TYPE2)
         (eq? (length (syntax->list CPARAMS1))
              (length (syntax->list CPARAMS2)))
         )))

;; > (ow-scheme-eq? #'(a b ()) #'(a b ()))            ; #t
;; > (ow-scheme-eq? #'(a b [c d e]) #'(a b [c d e]))  ; #t
;; > (ow-scheme-eq? #'(a b [c d e]) #'(a b [e d c]))  ; #f
;; > (ow-scheme-eq? #'(a b [c d e]) #'(a z [c d e]))  ; #f
;; > (ow-scheme-eq? #'(a b ()) #'(c b ()))            ; #f
(: ow-scheme-eq? (OW-SCHEME OW-SCHEME -> Boolean))
(define (ow-scheme-eq? ow1 ow2)
  (match-let ([(list TYPE1 OWNER1 CPARAMS1) (syntax-e ow1)]
              [(list TYPE2 OWNER2 CPARAMS2) (syntax-e ow2)])
    (and (bound-id=? TYPE1 TYPE2)
         (bound-id=? OWNER1 OWNER2)
         (for/and ([id1 (syntax->list CPARAMS1)]
                   [id2 (syntax->list CPARAMS2)])
           (bound-id=? id1 id2)))))

;; ;; (ids-eq? #'(a b c d) #'(a b c d))
;; ;; > #t
;; ;; (ids-eq? #'(a b c d) #'(d c b a))
;; ;; > #f
;; ;; (ids-eq? #'(a b c d) #'())
;; ;; > #f
;; (: ids-eq?
;;    ((Syntaxof (Listof Identifier)) (Syntaxof (Listof Identifier)) -> Boolean))
;; (define (ids-eq? IDS1 IDS2)
;;   (let ([IDs1 (syntax->list IDS1)]
;;         [IDs2 (syntax->list IDS2)])
;;     (for/and ([id1 IDs1][id2 IDs2])
;;       (eq? (syntax-e id1) (syntax-e id2)))))


;; Set or get the binder property of a syntax object.
;;
;; The binder property of a syntax object is another syntax object
;; which is its binder.
;; (: binder-prop
;;    (All (a) (case->
;;              [(Syntaxof a) -> BINDER]
;;              [(Syntaxof a) BINDER -> (Syntaxof a)])))
;; (define binder-prop
;;   (case-lambda
;;     ;; `(Syntaxof a) -> BINDER` requires a `cast` to type check
;;     ;; because the type-checker doesn't know that 'binder prop of
;;     ;; `syntax-property` stores BINDER.
;;     [(stx)        (cast (syntax-property  stx 'binder) BINDER)]
;;     [(stx BINDER) (syntax-property stx 'binder BINDER)]))

;; ;; Set or get the class type of a syntax object.
;; (: c-type-prop
;;    (All (a) (case->
;;              [(Syntaxof a) -> C-TYPE]
;;              [(Syntaxof a) C-TYPE -> (Syntaxof a)])))
;; (define c-type-prop
;;   (case-lambda
;;     [(stx)        (cast (syntax-property stx 'c-type) C-TYPE)]
;;     [(stx C-TYPE) (syntax-property stx 'c-type C-TYPE)]))


;; Get or set the basic type of a syntax object.
(: type-prop
   (All (a) (case->
             [(Syntaxof a) -> (U B-TYPE #f)]
             [(Syntaxof a) B-TYPE -> (Syntaxof a)])))
(define type-prop
  (case-lambda
    ;; Get the b-type of a syntax object.
    [(stx) (let ([τ (syntax-property stx 'b-type)])
             (and (b-type? τ) τ))]
    ;; Set the basic type of a syntax object
    [(stx B-TYPE) (syntax-property stx 'b-type B-TYPE)]))


;; Environments

;;~~~~~~~~~~~~~~~~~
;; CS: Set of Types

;; (: meta:CS (Boxof (Listof Identifier)))
;; (define meta:CS (box '()))

;; ;; Make `the-CS` the value of `CS` in `A`.
;; (: private:with-CS
;;    (All (A) (U (Syntaxof (Listof Identifier)) (Listof Identifier)) (-> A) -> A))
;; (define (private:with-CS the-CS thunk-E)
;;   (parameterize
;;       ([meta:CS
;;         (cond
;;           [(and (syntax? the-CS) (syntax->list the-CS)) => identity]
;;           [else the-CS])])
;;     ;; (dbg (CS))
;;     (thunk-E)))

;; ;; Automatically create the `thunk` around E expressions
;; (define-syntax (with-CS stx)
;;   (syntax-case stx ()
;;     [(_ THE-CS E ...) #'(private:with-CS THE-CS (thunk E ...))]))

(module+ test
  ;; (with-CS #'(foo bar)
  ;;   (check-true  (CS-member? #'foo))
  ;;   (check-false (CS-member? #'baz)))
  ;;
  ;; (with-CS (list #'foo #'bar)
  ;;   (check-true  (CS-member? #'foo))
  ;;   (check-false (CS-member? #'baz)))
  )

;;~~~~~~~~~~~~~~~~~~
;; FS: Map of fields
;;
;; With the syntax #'(Class-type . Field-name) as key and the Field
;; OW-SCHEME as value.
(define-type FS-key (Syntaxof (Pairof B-TYPE Identifier)))
(define-type FS (Dict FS-key OW-SCHEME))
;; (: meta:FS (Boxof FS))
;; (define meta:FS (box '()))

;; (: make-FS
;;    ((FS-key FS-key -> Boolean) ->
;;     (Values
;;      ;; dict-ref
;;      (FS-key -> OW-SCHEME)
;;      ;; dict-set
;;      (FS-key OW-SCHEME -> FS)
;;      ;; dict-has-key?
;;      (FS-key -> Boolean)
;;      ;; dict-eq?
;;      (FS (OW-SCHEME OW-SCHEME -> Boolean) -> Boolean))))
;; (define (make-FS fs-key=?) (make-dict meta:FS fs-key=?))


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

  ;; (: stx-eq? (OW-SCHEME OW-SCHEME -> Boolean))
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
                           (Syntaxof (Listof OW-SCHEME))  ; Type of def args
                           )))

(define-type DS (Dict DS-key OW-SCHEME))
;; (: meta:DS (Boxof DS))
;; (define meta:DS (box '()))

;; (define (ow-scheme=?))
;; (ds-key=? #'(a b ()) #'(a b ()))                  ; #t
;; (ds-key=? #'(a b [(c d ())]) #'(a b [(c d ())]))  ; #t
;; (ds-key=? #'(a b ()) #'(c d ()))                  ; #f
;; (ds-key=? #'(a b [(c d ())]) #'(a b [(d c ())]))  ; #f
(: ds-key=? ((OW-SCHEME OW-SCHEME -> Boolean) DS-key DS-key -> Boolean))
(define (ds-key=? ow-scheme=? key1-stx key2-stx)
  (match-let ([(list C-TYPE1 D-NAME1 ARGs-OW-SCHEME1) (syntax-e key1-stx)]
              [(list C-TYPE2 D-NAME2 ARGs-OW-SCHEME2) (syntax-e key2-stx)])
    (and
     (bound-id=? C-TYPE1 C-TYPE2)
     (bound-id=? D-NAME1 D-NAME2)
     (for/and ([ow1 (syntax->list ARGs-OW-SCHEME1)]
               [ow2 (syntax->list ARGs-OW-SCHEME2)])
       (ow-scheme=? ow1 ow2)))))

;; Same as `DS-key` except that is as a (Listof B-TYPE) instead of a
;; (Listof OW-SCHEME) as type of args
(define-type DS-key* (Syntaxof
                      (List Identifier                  ; Class type
                            Identifier                  ; Def name
                            (Syntaxof (Listof B-TYPE))  ; Type of def args
                            )))

(: dsk*->dsk (DS-key* -> DS-key))
(define (dsk*->dsk k*)
  (match-define (list class def b-types) (syntax-e k*))
  (define ows (map b-type->ow-scheme (syntax->list b-types)))
  (cast (datum->syntax k* (list class def ows) k*)
        DS-key)
  )

;; (: DS-member? ((OW-SCHEME OW-SCHEME -> Boolean) DS-key  -> Boolean))
;; (define (DS-member? ow-scheme=? ds-key)
;;   (dict-has-key? (meta:DS) ds-key (curry ds-key=? ow-scheme=?)))

;; (: DS-set ((OW-SCHEME OW-SCHEME -> Boolean) DS-key OW-SCHEME -> DS))
;; (define (DS-set ow-scheme=? ds-key OW-SCHEME)
;;   (dict-set (meta:DS) ds-key OW-SCHEME (curry ds-key=? ow-scheme=?)))

;; (: DS-ref ((OW-SCHEME OW-SCHEME -> Boolean) DS-key -> OW-SCHEME))
;; (define (DS-ref ow-scheme=? ds-key)
;;   (dict-ref (meta:DS) ds-key (curry ds-key=? ow-scheme=?)))


(define-predicate Γ-stx?
  (Syntaxof (Listof (Syntaxof (Pairof Identifier B-TYPE)))))



;; ;; Does not work
;; ;; See https://stackoverflow.com/a/60849727/2072144
;; (module desugar-def racket/base
;;   (require racket/set (only-in "utils.rkt" bound-id=?))

;;   (provide (rename-out [immutable-ids? ids?]
;;                        [make-immutable-ids make-ids]
;;                        [set-member? ids-member?]
;;                        [set-add ids-add]))

;;   (define-custom-set-types ids
;;     ;; This is not necessary because contract will check this
;;     ;; #:elem? identifier?
;;     bound-id=?)
;;   )

;; (require/typed/provide 'desugar-def
;;   [#:opaque Identifiers ids?]
;;   [make-ids ((Listof Identifier) -> Identifiers)]
;;   [ids-member? (Identifiers Identifier -> Boolean)]
;;   [ids-add (Identifiers Identifier -> Identifiers)])

;; ;; ;; (immutable-id-set? (make-immutable-id-set (list #'a #'b #'c)))
;; ;; ;; (immutable-id-set? #'(a b c)) ;; fails
;; ;; (set? #'(a b c))
;; ;; ;; (immutable-id-set? (list #'a #'b #'c))

;; (module UNTYPED racket/base
;;   (require  (only-in racket/set gen:set)
;;             racket/function)
;;   (provide (struct-out set))

;;   ;; : elems (Listof a)
;;   ;; : eql? (a a -> Boolean)
;;   (struct set [elems eql?]
;;     #:transparent
;;     #:constructor-name make-set
;;     #:methods gen:set
;;     [(define (set-member? st e)
;;        (let ([elems (set-elems st)]
;;              [eql?  (set-eql? st)])
;;          (findf (curry eql? e) elems)))
;;      (define (set-add st e)
;;        (let ([elems (set-elems st)]
;;              [eql?  (set-eql? st)])
;;          (if (set-member? st e) st
;;              (make-set (cons e elems) eql?))))]))

;; (require/typed 'UNTYPED
;;   [#:struct [a] set
;;    (#;[elems : (Listof a)]
;;     [eql?  : (Any Any -> Boolean)])
;;    #:constructor-name make-set
;;    ;; #:transparent
;;    ])

;; (module untyped racket/base
;;   (require racket/dict
;;            "utils.rkt")
;;   (provide (rename-out
;;             [make-immutable-id~>btype make-id~>btype])
;;            id~>btype?)

;;     ;; (HashTable Identifier B-TYPE)
;;     (define-custom-hash-types id~>btype
;;       #:key? identifier?
;;       (λ (x y) (eq? (syntax-e x) (syntax-e y))))
;;   )

;; (require 'untyped)
;; (provide (all-from-out 'untyped))

;; (unsafe-require/typed/provide 'untyped
;;  [#:opaque Id~>B-TYPE id~>btype?]
;;  [make-id~>btype ((Listof (Pairof Identifier B-TYPE)) -> Id~>B-TYPE)])

;; (id~>btype? #f)
;; (id~>btype? #'f)
;; (make-id~>btype (list (cons #'foo 'a)))
