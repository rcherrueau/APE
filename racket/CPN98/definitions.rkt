#lang typed/racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;;
;; Common definitions for phases of the lang.

(require (for-syntax racket/base)
         racket/list
         racket/match
         racket/string
         typed/racket/unsafe)

(require/typed "utils.rkt"
  [check-stx=? ((Syntax Syntax) (String) . ->* . Any)]
  [zip (All (a b) ((Listof a) (Listof b) -> (Listof (Pairof a b))))]
  [unzip (All (a b) ((Listof (Pairof a b)) -> (Values (Listof a) (Listof b))))])

(unsafe-require/typed "utils.rkt"
  [mk-ow-type-surface (TYPE OWNER CPARAMS -> Syntax)]
  [set-surface-stx (All (a) (Syntaxof a) Syntax -> (Syntaxof a))]
  [get-surface-stx (Syntax -> Syntax)])

(require/typed racket/list
  [index-of (All (a) ((Listof a) a (a a -> Boolean) -> (U Nonnegative-Integer #f)))])

(unsafe-require/typed racket/sequence
  [in-syntax (All (a) (Syntaxof (Listof (Syntaxof a))) -> (Sequenceof (Syntaxof a)))])

(provide (except-out (all-defined-out)
                     mk-ow-type
                     def-ow-type-values
                     private:alist-index-of
                     alist-set)
         (all-from-out 'definition/untyped))

(unsafe-provide mk-ow-type def-ow-type-values)

(module+ test (require typed/rackunit))


;; Ownership type syntax object

;; Definitions
(define-type TYPE Identifier)                               ;; Basic type #'Int, #'Bool, ...
(define-type OWNER Identifier)                              ;; Owner #'rep, #'world, #'Θ, #'o, ...
(define-type CPARAMS (Syntaxof (Listof Identifier)))        ;; List of ctx params #'(), #'(rep n)
(define-type OW-TYPE (Syntaxof (List TYPE OWNER CPARAMS)))  ;; Ownership type #'(Foo rep {n m})

;; Predicate for occurrence typing, see [LLNC19] and
;; https://docs.racket-lang.org/ts-guide/occurrence-typing.html
(define-predicate type? TYPE)
(define-predicate ow-type? OW-TYPE)

;; Make an ownership type
(: mk-ow-type
   ((TYPE OWNER (U CPARAMS (Listof Identifier)))
    [#:surface Syntax]
    . ->* . OW-TYPE))
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


;; The identifier is a local one and does not come from a module.
(: is-local-id? (Identifier -> Boolean))
(define (is-local-id? id)
  (string-contains? (symbol->string (syntax-e id)) "."))



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


;; Returns #t if two identifiers are identical
(: bound-id=? (Identifier Identifier -> Boolean))
(define (bound-id=? id1 id2)
  (eq? (syntax-e id1) (syntax-e id2)))


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


;; TODO: clean this

;;~~~~~~~~~~~~~~~~~
;; CS: Set of Types
(define-type CS (Listof (Pairof TYPE                    ; class type
                                (Listof Identifier))))  ; context parameters

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


;; Untyped common definitions for phases of the lang.  Generally
;; speaking, stuffs that encompass syntax objects are hard to type in
;; racket.  So, I make them to land on here, in the untyped module.  I
;; should -- as typed/racket evolved -- move definition from this
;; untyped module to the typed one.
(module definition/untyped racket/base
  (require (for-syntax racket/base
                       racket/string
                       racket/syntax
                       syntax/stx
                       syntax/parse/define
                       "utils.rkt")
           syntax/parse
           syntax/parse/define
           "utils.rkt")

  (provide (all-defined-out))

  ;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;;; Module language def

  ;; Literals for classes definition
  (define-literal-set keyword-lits
    #:for-label
    ;; Note: The syntax/parse package require all literals to have a
    ;; binding. To match identifier by their symbolic names, I have to
    ;; use `#:datum-literals` instead.
    #:datum-literals (import class field def)
    ;; I have no literals that should have a binding.
    ())

  ;; Literals for expressions definition
  (define-literal-set expr-lits
    #:for-label
    #:datum-literals (let new send get-field set-field! this ???)
    ())

  ;; Literals for types definition
  (define-literal-set type-lits
    #:for-label
    #:datum-literals (-> → :)
    ())

  ;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;;; Syntax class

  ;; Ownership type syntax
  ;;
  ;; TODO: ensure consistency with OW-TYPE, mk-ow-type, ow-type-values
  ;; > (check-true (syntax-parse (mk-ow-type type owner {}) [_:ow-type] #t))
  (define-syntax-class ow-type
    #:description "type with ownership and context parameters"
    #:attributes [TYPE OWNER CPARAMS]
    #:commit
    #:opaque
    (pattern (TYPE:id OWNER:id (CPARAM:id ...))
             #:with CPARAMS #'(CPARAM ...)))

  ;; TODO: add path syntax class for modules

  ;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;;; Utils

  ;; Macro to define a new phase of the language
  ;;
  ;; A phase is a transformation of or an analyze on the syntax tree
  ;; of the language. It takes a syntax object as an argument (i.e,
  ;; the AST) and returns the possibly modified syntax object after
  ;; transformation/analysis. Every environment variable is
  ;; dynamically bound (i.e., racket parameter) in the /phase
  ;; definition/. A constructor, named `with-E`, lets the programmer
  ;; sets a new value to the environment, similarly to racket
  ;; `parameterize`.
  ;;
  ;; > (define-phase (?> ast-stx arg1 arg2 ...)
  ;; >   ;; Environment variables (racket parameter -- dynamic binding)
  ;; >   [E:env ...]
  ;; >
  ;; >   ;; Phase definition
  ;; >   E:expr)
  (begin-for-syntax
    ;; Returns #t if a syntax object is of the form #'env:the-id
    ;; (i.e., an identifier that starts with `env:`), or #f otherwise.
    ;;
    ;; (: is-stx-env:the-id? (Identifier -> Boolean))
    (define (is-stx-env:the-id? stx)
      (string-prefix? (symbol->string (syntax-e stx)) "env:"))

    ;; Strips the `env:` part of a #'env:the-id syntax object
    ;;
    ;; (: env:the-id->the-id (Identifier -> Identifier))
    (define (env:the-id->the-id stx)
      (define the-id (string-trim (symbol->string (syntax-e stx))
                                  "env:" #:right? #f))
      (format-id stx "~a" the-id #:source stx))

    (define-syntax-class env
      #:description "an environment variable"
      #:attributes [NAME MAKER INIT-VAL DEFS WITH]
      #:commit
      (pattern (NAME:id #:init INIT-VAL:expr
                        #:mk MAKER:expr)
               #:attr DEFS #'()
               #:attr WITH (format-id #'NAME "with-~a" (syntax-e #'NAME))
               )
      (pattern (NAME:id #:init INIT-VAL:expr
                        #:mk MAKER:expr
                        #:apply? [envDEF:id ...])
               ;; Check that all def that should be applied to the env starts with `env:`
               #:fail-unless (stx-for/and ([id #'(envDEF ...)])
                                          (is-stx-env:the-id? id))
               "expected an identifier that starts with `env:`"
               #:with [DEF-APPLY-NAME ...] (stx-map env:the-id->the-id #'(envDEF ...))
               #:attr DEFS #'((envDEF DEF-APPLY-NAME) ...)
               #:attr WITH (format-id #'NAME "with-~a" (syntax-e #'NAME))
               )))

  (define-syntax (define-phase parser-stx)
    (define env->name/def/apply
      (syntax-parser
        [e:env
         #:with [(e-def e-apply) ...] #'e.DEFS
         #'((e.NAME e-def e-apply) ...)]))

    (syntax-parse parser-stx
      [(_
        ;; Phase name (e.g., `?>`), its syntax object and arguments
        (ID:id stx:expr ARG:expr ...)
        ;; Environment variables ...
        E:env ...
        ;; Phase definition
        DEF:expr)
       #:with ((E-NAME E-DEF E-APPLY) ...)
       (stx-flatten (stx-map env->name/def/apply #'(E ...)))
       #'(begin
           ;; Define environment variables as parameters
           ;; (: E.NAME (Parameterof E.TYPE)) ...
           (define E.NAME (make-parameter #f)) ...

           ;; The with-param macro
           (... ;; Note: A stat-template is like a template, except that
            ;; ..., ~?, and ~@ are interpreted as constants instead
            ;; of template forms. See
            ;; https://docs.racket-lang.org/reference/stx-patterns.html#%28form._%28%28lib._racket%2Fprivate%2Fstxcase-scheme..rkt%29._syntax%29%29
            (define-simple-macro (E.WITH THE-PARAM EXPR:expr ...+)
              (parameterize ([E.NAME (E.MAKER THE-PARAM)]) EXPR ...)))
           ...

           ;; Partially applied DEF with E-NAME parameter
           (define (E-APPLY . args) (apply E-DEF (E-NAME) args)) ...

           ;; Define the parser as a global definition. We parameterize
           ;; the first call of `DEF` with `Env` values
           (define (ID stx ARG ...)
             (parameterize ([E.NAME (E.MAKER E.INIT-VAL)] ...) DEF)))]
      ))

  (define-syntax define-rules
    (syntax-parser
      [(_ ID:id RULE:expr ...)
       #'(define (ID stx)
           ;; (dbg stx #:ctx ID)
           (syntax-parse stx
             #:literal-sets [(keyword-lits #:at ID)
                             (expr-lits #:at ID)
                             (type-lits #:at ID)]
             RULE ...
             ))])))

(require 'definition/untyped)


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
