#lang typed/racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;;
;; Environments def for parsers of the lang.
;;
;; In phases (e.g., desugar.rkt) I have to go with untyped racket
;; because it relies to much on `syntax-parse` and consœurs which lack
;; of good definitions in typed/racket.  But in general I prefer typed
;; code.  Typed code prevents to systematically tangle annoying
;; input-validation code in your function.
;;
;; In phases, managing env should have be done in typed/racket because
;; it only involves operations on list or hash map. Unfortunately, I
;; find it really hard to mix untyped code with typed one... The
;; `with-type` construction is not a good solution.  It doesn't
;; benefit from type definitions outside of it (that I use a lot with
;; `typed/no-check` to document my code).  So I have to write type
;; annotation twice (outside `with-type` for documentation and inside
;; `with-type` for type checking).
;;
;; To circumvent this -- and keep the code as clean as possible -- I
;; put environments definition in this typed module. Here, I relies on
;; `unsafe-require/typed` and then provides a safe interface.

(require racket/function
         racket/list
         racket/match
         racket/sequence
         syntax/parse
         typed/racket/unsafe
         "definitions.rkt"
         "utils.rkt")

(require/typed "utils.rkt"
  [check-stx=? ((Syntax Syntax) (#:msg String) . ->* . Any)]
  [zip (All (a b) ((Listof a) (Listof b) -> (Listof (Pairof a b))))]
  [unzip (All (a b) ((Listof (Pairof a b)) -> (Values (Listof a) (Listof b))))]
  [*** (All (a b c d) ((a -> b) (c -> d) -> ((Pairof a c) -> (Pairof b d))))]
  )

(unsafe-require/typed racket/sequence
  [in-syntax (All (a) ((Syntaxof (Listof a)) -> (Sequenceof a)))])


(module+ test (require typed/rackunit))


;; General algebraic structures

;;; Set

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

;; Produces a set that includes elements of `st1` and `st2`.
(: set-union (All [a] ((Setof a) (Setof a) -> (Setof a))))
(define (set-union st1 st2)
  (foldr (λ ([elem : a]
             [the-set : (Setof a)])
           (set-add the-set elem)) st1 (set-elems st2)))


(module+ desugar
  (provide (all-defined-out))
  (module+ test (require typed/rackunit))

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
      [(syntax? ids) (_make-Γ (syntax->list ids))]
      ;; Do nothing if its already a (Setof Identifier)
      [else ids]))

  ;; Is VAR bounded in Γ?
  (: Γ-member? ((Setof Identifier) Identifier -> Boolean))
  (define (Γ-member? Γ VAR) (set-member? Γ VAR))

  ;; Add a VAR to Γ
  (: Γ-add ((Setof Identifier) Identifier -> (Setof Identifier)))
  (define (Γ-add Γ VAR) (set-add Γ VAR))

  (module+ test
    (provide (all-defined-out))

    (define Γ-tests
      (test-suite "Test for Γ env"
       (for ([Γ (in-list (list (make-Γ (list #'foo #'bar))
                               (make-Γ #'(foo bar))
                               (make-Γ (make-Γ (list #'foo #'bar)))
                               (make-Γ (make-Γ #'(foo bar)))))])
         (check-true  (Γ-member? Γ #'foo))
         (check-true  (Γ-member? Γ #'bar))
         (check-false (Γ-member? Γ #'baz))
         (check-true  (Γ-member? (Γ-add Γ #'baz) #'baz))
         (check-false (Γ-member? Γ #'baz)))))
    ))


;; basic-check

(module+ basic-check
  (provide (all-defined-out))
  (module+ test (require typed/rackunit))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; CS is the set of defined types
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ;; Make a new CS
  (: make-CS ((Listof B-TYPE) -> (Setof B-TYPE)))
  (define (make-CS ids) (make-set ids bound-id=?))

  ;; Is VAR bounded in CS?
  (: CS-member? ((Setof B-TYPE) B-TYPE -> Boolean))
  (define (CS-member? CS VAR) (set-member? CS VAR))

  (module+ test
    (provide (all-defined-out))
    (define CS (make-CS (list #'foo #'bar)))
    (define CS-tests
      (test-suite "Test for CS env"
       (check-true  (CS-member? CS #'foo))
       (check-true  (CS-member? CS #'bar))
       (check-false (CS-member? CS #'baz))))
    )

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Γ is the map of locally bound variables
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ;; Untyped world
  (module untyped-Γ racket/base
    (require racket/dict "definitions.rkt")
    (provide (rename-out
              [immutable-Γ?     u:Γ?]
              [make-immutable-Γ u:make-Γ]
              [dict-has-key?    u:Γ-has-key?]
              [dict-set         u:Γ-set]
              [dict-ref         u:Γ-ref]))

    (define-custom-hash-types Γ bound-id=?))

  (unsafe-require/typed 'untyped-Γ
    [#:opaque Γ u:Γ?]
    [u:make-Γ     ((Listof (Pairof Identifier B-TYPE)) -> Γ)]
    [u:Γ-has-key? (Γ Identifier -> Boolean)]
    [u:Γ-set      (Γ Identifier B-TYPE -> Γ)]
    [u:Γ-ref      (Γ Identifier -> B-TYPE)])

  (define-predicate listof-Γs?
    (Listof (Pairof Identifier B-TYPE)))

  (define-predicate stxof-Γs?
    (Syntaxof (Listof (Syntaxof (Pairof Identifier B-TYPE)))))

  ;; Make a new `Γ`
  (: make-Γ (() ((U (Listof (Pairof Identifier B-TYPE))
                    (Syntaxof (Listof (Syntaxof (Pairof Identifier B-TYPE))))
                    Γ)) . ->* . Γ))
  (define (make-Γ [ids '()])
    (cond
      [(listof-Γs? ids) (u:make-Γ ids)]
      ;; Look at https://docs.racket-lang.org/typed-map/index.html?q=map
      [(stxof-Γs? ids)
       (u:make-Γ (map (inst syntax-e (Pairof Identifier B-TYPE))
                      (syntax->list ids)))]
      ;; Do nothing if its already a Γ
      [else ids]))

  ;; Is `VAR` bound in `Γ`
  (: Γ-member? (Γ Identifier -> Boolean))
  (define Γ-member? u:Γ-has-key?)

  ;; Set `TYPE` of `VAR` in `Γ`
  (: Γ-add (Γ (Syntaxof (Pairof Identifier B-TYPE)) -> Γ))
  (define (Γ-add Γ VAR.TYPE)
    (match-define (cons VAR TYPE) (syntax-e VAR.TYPE))
    (u:Γ-set Γ VAR TYPE))

  ;; Returns the `TYPE` of `VAR` in `Γ`
  (: Γ-ref (Γ Identifier -> B-TYPE))
  (define Γ-ref u:Γ-ref)

  (module+ test
    (provide (all-defined-out))

    (define listof-id~>bytes (list (cons #'foo #'bar)
                                   (cons #'fizz #'buzz)))
    (define stxof-id~>bytes #'{ (foo . bar)
                                (fizz . buzz) })
    (define Γ-tests
      (test-suite "Test for Γ env"
       (for ([Γ (in-list (list (make-Γ listof-id~>bytes)
                               (make-Γ stxof-id~>bytes)
                               (make-Γ (make-Γ listof-id~>bytes))
                               (make-Γ (make-Γ stxof-id~>bytes))))])
         (check-true  (Γ-member? Γ #'foo))
         (check-stx=? (Γ-ref Γ #'foo) #'bar)
         (check-true  (Γ-member? Γ #'fizz))
         (check-stx=? (Γ-ref Γ #'fizz) #'buzz)
         (check-false (Γ-member? Γ #'baz))
         (let ([updated-test:Γ  (Γ-add Γ #'(foo  . baz))]
               [inserted-test:Γ (Γ-add Γ #'(toto . tata))])
           (check-true  (Γ-member? updated-test:Γ #'foo))
           (check-stx=? (Γ-ref updated-test:Γ #'foo) #'baz)
           (check-true  (Γ-member? inserted-test:Γ #'toto))
           (check-stx=? (Γ-ref inserted-test:Γ #'toto) #'tata))))
      ))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; FS is the map of fields, with the syntax #'(Class-type
  ;; . Field-name) as key and the Field type as value.
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ;; Untyped world
  (module untyped-FS racket/base
    (require racket/dict racket/match
             "definitions.rkt")
    (provide (rename-out
              [immutable-FS?     u:FS?]
              [make-immutable-FS u:make-FS]
              [dict-has-key?     u:FS-has-key?]
              [dict-ref          u:FS-ref]))

    ;; TODO: put this in definition.rkt
    ;;
    ;; FS key equality:
    ;;
    ;; (key=? #'(a . b) #'(a . b))  ; #t
    ;; (key=? #'(a . b) #'(c . d))  ; #f
    ;; (key=? #'(a . 1) #'(a . 1))  ; #f
    (define (key=? key1-stx key2-stx)
      (match-let ([(cons C-TYPE1 F-NAME1) (syntax-e key1-stx)]
                  [(cons C-TYPE2 F-NAME2) (syntax-e key2-stx)])
        (and (bound-id=? C-TYPE1 C-TYPE2)
             (bound-id=? F-NAME1 F-NAME2))))

    (define-custom-hash-types FS key=?))

  (define-type FS-key (Syntaxof (Pairof B-TYPE Identifier)))

  (unsafe-require/typed 'untyped-FS
    [#:opaque FS u:FS?]
    [u:make-FS     ((Listof (Pairof FS-key B-TYPE)) -> FS)]
    [u:FS-has-key? (FS FS-key -> Boolean)]
    [u:FS-ref      (FS FS-key -> B-TYPE)])

  ;; Make FS
  (: make-FS ((Listof (Pairof FS-key B-TYPE)) -> FS))
  (define make-FS u:make-FS)

  ;; Is `Class-type . Field-name` member of `FS`.
  (: FS-member? (FS FS-key -> Boolean))
  (define FS-member? u:FS-has-key?)

  ;; Returns the `TYPE` of `Field-name` in `Class-type`.
  (: FS-ref (FS FS-key -> B-TYPE))
  (define FS-ref u:FS-ref)

  (module+ test
    (provide (all-defined-out))

    (define FS (make-FS (list (cons #'(class . field) #'t)
                              (cons #'(clazz . dleif) #'u))))

    (define FS-tests
      (test-suite "Test for FS env"
       (check-true  (FS-member? FS #'(class . field)))
       (check-true  (FS-member? FS #'(clazz . dleif)))
       (check-false (FS-member? FS #'(Class . field)))
       (check-false (FS-member? FS #'(class . Field)))

       (check-stx=? (FS-ref FS #'(class . field)) #'t)
       (check-stx=? (FS-ref FS #'(clazz . dleif)) #'u))
      ))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; DS is the map of definitions, with the syntax #'(Class-type
  ;; Def-name (Def-arg-type ...)) as key and the def return type as
  ;; value.
  ;;
  ;; Note: I go with a Hastable were DS keys are B-TYPE instead of
  ;; OW-SCHEME because basic-check only take care of B-TYPE.
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ;; Untyped world
  (module untyped-DS racket/base
    (require racket/dict racket/match
             "definitions.rkt")
    (provide (rename-out
              [immutable-DS?     u:DS?]
              [make-immutable-DS u:make-DS]
              [dict-has-key?     u:DS-has-key?]
              [dict-ref          u:DS-ref]
              [dict-keys         u:DS-domain]))

    ;; Two DS-key are equals iff:
    ;;;; Defined in the same class.
    (define c-type=? bound-id=?)
    ;;;; Same name.
    (define d-name=? bound-id=?)
    ;;;; Same number of arguments.
    (define (num-args=? args1 args2) (eq? (length args1) (length args2)))
    ;;;; Same argument types (type comparison does not look at the
    ;;;; owner or context parameter).
    (define b-type=? bound-id=?)
    ;;;; Putting all together
    ;;;; (: ds-key=? (DS-key DS-key -> Boolean))
    (define (ds-key=? key1-stx key2-stx)
      (match-let* ([(list C-TYPE1 D-NAME1 ARGs-B-TYPE1) (syntax-e key1-stx)]
                   [(list C-TYPE2 D-NAME2 ARGs-B-TYPE2) (syntax-e key2-stx)]
                   [b-types1 (syntax->list ARGs-B-TYPE1)]
                   [b-types2 (syntax->list ARGs-B-TYPE2)])
        (and
         (c-type=? C-TYPE1 C-TYPE2)
         (d-name=? D-NAME1 D-NAME2)
         (num-args=? b-types1 b-types2)
         (for/and ([bt1 (in-list b-types1)] [bt2 (in-list b-types2)])
           (b-type=? bt1 bt2)))))

    (define-custom-hash-types DS ds-key=?))

  (unsafe-require/typed 'untyped-DS
    [#:opaque DS u:DS?]
    [u:make-DS     ((Listof (Pairof DS-key B-TYPE)) -> DS)]
    [u:DS-has-key? (DS DS-key -> Boolean)]
    [u:DS-ref      (DS DS-key -> B-TYPE)]
    [u:DS-domain   (DS -> (Listof DS-key))])

  ;; Same as `DS-key` except that is as a (Listof B-TYPE) instead of a
  ;; (Listof OW-SCHEME) as type of args
  (define-type DS-key (Syntaxof
                         (List Identifier                  ; Class type
                               Identifier                  ; Def name
                               (Syntaxof (Listof B-TYPE))  ; Type of def args
                               )))

  ;; Make DS
  (: make-DS ((Listof (Pairof DS-key B-TYPE)) -> DS))
  (define make-DS u:make-DS)

  ;; Is `#'(Class-type Def-name (Def-args ...))` member of `DS`.
  (: DS-member? (DS DS-key -> Boolean))
  (define DS-member? u:DS-has-key?)

  ;; Returns the `TYPE` of `Field-name` in `Class-type`.
  (: DS-ref (DS DS-key -> B-TYPE))
  (define DS-ref u:DS-ref)

  ;; Return the domain of DS
  (: DS-domain (DS -> (Listof DS-key)))
  (define DS-domain u:DS-domain)

  (module+ test
    (provide (all-defined-out))

    (define DS (make-DS (list (cons #'(class def1 ())    #'t)
                              (cons #'(clazz def1 ())    #'u)
                              (cons #'(class def2 (t u)) #'v))))

    (define DS-tests
      (test-suite "Test for DS env"
       (check-true  (DS-member? DS #'(class def1 ())))
       (check-true  (DS-member? DS #'(clazz def1 ())))
       (check-true  (DS-member? DS #'(class def2 (t u))))
       (check-false (DS-member? DS #'(Class def1 ())))
       (check-false (DS-member? DS #'(class Def1 ())))
       (check-false (DS-member? DS #'(class def1 (t)))     "Arity mismatch")
       (check-false (DS-member? DS #'(class def2 (t)))     "Arity mismatch")
       (check-false (DS-member? DS #'(class def2 (t u v))) "Arity mismatch")
       (check-false (DS-member? DS #'(class def2 (t v)))   "Type mismatch")

       (check-stx=? (DS-ref DS #'(class def1 ())) #'t)
       (check-stx=? (DS-ref DS #'(clazz def1 ())) #'u)
       (check-stx=? (DS-ref DS #'(class def2 (t u))) #'v)
       )))
  )


;; ownership

(module+ ownership
  (provide (all-defined-out))
  (module+ test (require typed/rackunit))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Σ is the set of local context parameters
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  (define-predicate listof-ids? (Listof Identifier))

  ;; Make a new Σ
  (: make-Σ (() ((U (Listof Identifier)
                    (Syntaxof (Listof Identifier))
                    (Setof Identifier))) . ->* . (Setof Identifier)))
  (define (make-Σ [ids '()])
    (define (_make-Σ [_ids : (Listof Identifier)])
      (make-set _ids bound-id=?))

    (cond
      [(listof-ids? ids) (_make-Σ ids)]
      [(syntax? ids) (_make-Σ (syntax->list ids))]
      ;; Do nothing if its already a (Setof Identifier)
      [else ids]))

  ;; Is VAR bounded in Σ?
  (: Σ-member? ((Setof Identifier) Identifier -> Boolean))
  (define (Σ-member? Σ VAR) (set-member? Σ VAR))

  ;; Add a CTX to Σ
  (: Σ-add ((Setof Identifier) Identifier -> (Setof Identifier)))
  (define Σ-add set-add)

  ;; Union two sets
  (: Σ-union ((Setof Identifier)
              (U (Setof Identifier)
                 (Syntaxof (Listof Identifier)))
              -> (Setof Identifier)))
  (define (Σ-union Σ the-set)
    (cond
      [(syntax? the-set)
       (foldr (λ ([id : Identifier]
                  [the-set : (Setof Identifier)])
                (set-add the-set id)) Σ (syntax->list the-set))]
      [else (set-union Σ the-set)]))

  (module+ test
    (provide (all-defined-out))

    (define Σ-tests
      (test-suite "Test for Σ env"
       (for ([Σ (in-list (list (make-Σ (list #'foo #'bar))
                               (make-Σ #'(foo bar))
                               (make-Σ (make-Σ (list #'foo #'bar)))
                               (make-Σ (make-Σ #'(foo bar)))))])
         (check-true  (Σ-member? Σ #'foo))
         (check-true  (Σ-member? Σ #'bar))
         (check-true  (Σ-member? (Σ-add Σ #'baz) #'baz))
         (check-true  (Σ-member? (Σ-union Σ #'(baz buz)) #'baz))
         (check-true  (Σ-member? (Σ-union Σ #'(baz buz)) #'buz))
         (check-false (Σ-member? Σ #'baz))
         (check-false (Σ-member? Σ #'buz))
         ))))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; OWS is the mapping from class types to its ownership scheme
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ;; Untyped world
  (module untyped-OWS racket/base
    (require racket/dict racket/match
             "definitions.rkt")
    (provide (rename-out
              [immutable-OWS?     u:OWS?]
              [make-immutable-OWS u:make-OWS]
              [dict-has-key?      u:OWS-has-key?]
              [dict-ref           u:OWS-ref]))

    (define-custom-hash-types OWS bound-id=?))

  (unsafe-require/typed 'untyped-OWS
    [#:opaque OWS u:OWS?]
    [u:make-OWS     ((Listof (Pairof Identifier OW-SCHEME)) -> OWS)]
    [u:OWS-has-key? (OWS Identifier -> Boolean)]
    [u:OWS-ref      (OWS Identifier -> OW-SCHEME)])

  ;; Make OWS
  (: make-OWS ((Listof (Pairof Identifier OW-SCHEME)) -> OWS))
  (define make-OWS u:make-OWS)

  ;; Get the ownership scheme of a class
  (: OWS-ref (OWS Identifier -> OW-SCHEME))
  (define OWS-ref u:OWS-ref)

  ;; Number of context parameters taken by a specific class
  (: OWS-arity (OWS Identifier -> Nonnegative-Integer))
  (define (OWS-arity OWS ctype)
    (let* ([ows       (OWS-ref OWS ctype)]
           [cparam... (ow-scheme->c-params ows)])
      (length (syntax->list cparam...))))

  ;; Construct a substitution table for σ
  (: ψ (OWS OW-SCHEME -> (Identifier . ~> . Identifier)))
  (define (ψ OWS otype)
    ;; Destruct the ownership type
    (match-define (list ot.TYPE ot.OWNER ot.CPARAMS) (strip-ow-scheme otype))

    ;; Find its ownership scheme and destruct it
    (match-define (list os.TYPE os.OWNER os.CPARAMS)
      (let ([ow-scheme (OWS-ref OWS ot.TYPE)])
        (strip-ow-scheme ow-scheme)))

    ;; Make the σ
    (cons
     (cons os.OWNER ot.OWNER)
     (for/list : (Identifier . ~> . Identifier)
         ([OS-CPARAM : Identifier (in-syntax os.CPARAMS)]
          [OT-CPARAM : Identifier (in-syntax ot.CPARAMS)])
       (cons OS-CPARAM OT-CPARAM))))

  ;; Substitute element of one scheme using a substitution table
  (: σ ((Identifier . ~> . Identifier) OW-SCHEME -> OW-SCHEME))
  (define (σ subs-table ow-scheme)
    (match-define (list os.TYPE os.OWNER os.CPARAMS)
      (strip-ow-scheme ow-scheme))

    (let ([b-type : B-TYPE os.TYPE]
          [owner  : OWNER ((inst alist-ref Identifier Identifier)
                           subs-table os.OWNER bound-id=? identity)]
          [cparams : (Listof Identifier)
                   ((inst map Identifier Identifier)
                    (λ (id) ((inst alist-ref Identifier Identifier)
                             subs-table id bound-id=? identity))
                    (syntax->list os.CPARAMS))])
      (mk-ow-scheme b-type owner cparams)))


  (module+ test
    (provide (all-defined-out))

    (: datumify ((Identifier . ~> . Identifier) -> (Sexp . ~> . Sexp)))
    (define (datumify xs)
      (alist-map-w/key (syntax->datum . *** . syntax->datum) xs))

    (define OWS (make-OWS `((,#'Foo . ,#'(Foo Θ ()))
                            (,#'Bar . ,#'(Bar Θ (ν μ))))))

    (define OWS-tests
      (test-suite "Test for OWS env"
        (check-stx=? (OWS-ref OWS #'Foo) #'(Foo Θ ()))
        (check-stx=? (OWS-ref OWS #'Bar) #'(Bar Θ (ν μ)))
        (check-eq? (OWS-arity OWS #'Foo) 0)
        (check-eq? (OWS-arity OWS #'Bar) 2)
        (check-not-eq? (OWS-arity OWS #'Bar) 0)
        (check-exn exn? (thunk (OWS-arity OWS #'Baz)))

        ;; ψ
        (check-equal? (datumify (ψ OWS #'(Foo o ())))
                      (datumify `((,#'Θ . ,#'o))))
        (check-equal? (datumify (ψ OWS #'(Bar o (n m))))
                      (datumify `((,#'Θ . ,#'o) (,#'ν . ,#'n) (,#'μ . ,#'m))))


        ;; Check σ('(), t) = t
        (check-stx=? (σ '() #'(Foo o ()))    #'(Foo o ()))
        (check-stx=? (σ '() #'(Bar o (n n))) #'(Bar o (n n)))
        ;; Check σ(ψ(t), t^) = t   ;; t^ is the ownership scheme of t
        (check-stx=? (σ (ψ OWS #'(Foo o ()))    #'(Foo Θ ()))    #'(Foo o ()))
        (check-stx=? (σ (ψ OWS #'(Bar o (n m))) #'(Bar Θ (ν μ))) #'(Bar o (n m)))
        ;; ...
        (check-stx=? (σ `((,#'Θ . ,#'o)) #'(Foo Θ ())) #'(Foo o ()))
        (check-stx=? (σ `((,#'Θ . ,#'o)) #'(Foo z ())) #'(Foo z ()))
        (check-stx=? (σ `((,#'ν . ,#'n)) #'(Bar ν (ν ν))) #'(Bar n (n n)))
        (check-stx=? (σ `((,#'ν . ,#'n)) #'(Bar o (ν m))) #'(Bar o (n m)))
        )))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Γ is the mapping of locally bound variables to their ownership type
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ;; Untyped world
  (module untyped-Γ racket/base
    (require racket/dict "definitions.rkt")
    (provide (rename-out
              [immutable-Γ?     u:Γ?]
              [make-immutable-Γ u:make-Γ]
              [dict-has-key?    u:Γ-has-key?]
              [dict-set         u:Γ-set]
              [dict-ref         u:Γ-ref]))

    ;; TODO: ensure bound-id=? is what I need here
    (define-custom-hash-types Γ bound-id=?))

  (unsafe-require/typed 'untyped-Γ
    [#:opaque Γ u:Γ?]
    [u:make-Γ     ((Listof (Pairof Identifier OW-SCHEME)) -> Γ)]
    [u:Γ-has-key? (Γ Identifier -> Boolean)]
    [u:Γ-set      (Γ Identifier OW-SCHEME -> Γ)]
    [u:Γ-ref      (Γ Identifier -> OW-SCHEME)])

  (define-predicate listof-Γs?
    (Listof (Pairof Identifier OW-SCHEME)))

  (define-predicate stxof-Γs?
    (Syntaxof (Listof (Syntaxof (Pairof Identifier OW-SCHEME)))))

  ;; Make a new `Γ`
  (: make-Γ (() ((U (Listof (Pairof Identifier OW-SCHEME))
                    (Syntaxof (Listof (Syntaxof (Pairof Identifier OW-SCHEME))))
                    Γ)) . ->* . Γ))
  (define (make-Γ [ids '()])
    (cond
      [(listof-Γs? ids) (u:make-Γ ids)]
      ;; Look at https://docs.racket-lang.org/typed-map/index.html?q=map
      [(stxof-Γs? ids)
       (u:make-Γ (map (inst syntax-e (Pairof Identifier OW-SCHEME))
                      (syntax->list ids)))]
      ;; Do nothing if its already a Γ
      [else ids]))

  ;; Is `VAR` bound in `Γ`
  (: Γ-member? (Γ Identifier -> Boolean))
  (define Γ-member? u:Γ-has-key?)

  ;; Set `TYPE` of `VAR` in `Γ`
  (: Γ-add (Γ (Syntaxof (Pairof Identifier OW-SCHEME)) -> Γ))
  (define (Γ-add Γ VAR.TYPE)
    (match-define (cons VAR TYPE) (syntax-e VAR.TYPE))
    (u:Γ-set Γ VAR TYPE))

  ;; Returns the `TYPE` of `VAR` in `Γ`
  (: Γ-ref (Γ Identifier -> OW-SCHEME))
  (define Γ-ref u:Γ-ref)

  (module+ test
    (provide (all-defined-out))

    (define listof-id~>bytes (list (cons #'foo  #'(t o ()))
                                   (cons #'fizz #'(t o (c1 c2)))))
    (define stxof-id~>bytes #'{ (foo  . (t o ()))
                                (fizz . (t o (c1 c2))) })
    (define Γ-tests
      (test-suite "Test for Γ env"
       (for ([Γ (in-list (list (make-Γ listof-id~>bytes)
                               (make-Γ stxof-id~>bytes)
                               (make-Γ (make-Γ listof-id~>bytes))
                               (make-Γ (make-Γ stxof-id~>bytes))))])
         (check-true  (Γ-member? Γ #'foo))
         (check-stx=? (Γ-ref Γ #'foo) #'(t o ()))
         (check-true  (Γ-member? Γ #'fizz))
         (check-stx=? (Γ-ref Γ #'fizz) #'(t o (c1 c2)))
         (check-false (Γ-member? Γ #'baz))
         (let ([updated-test:Γ  (Γ-add Γ #'(foo  . (t o (c1 c2))))]
               [inserted-test:Γ (Γ-add Γ #'(toto . (t o (c1 c2))))])
           (check-true  (Γ-member? updated-test:Γ #'foo))
           (check-stx=? (Γ-ref updated-test:Γ #'foo) #'(t o (c1 c2)))
           (check-true  (Γ-member? inserted-test:Γ #'toto))
           (check-stx=? (Γ-ref inserted-test:Γ #'toto) #'(t o (c1 c2))))))
      ))


  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; FS is the map of fields, with the syntax #'(Class-type
  ;; . Field-name) as key and the Field ownership type as value.
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ;; Untyped world
  (module untyped-FS racket/base
    (require racket/dict racket/match
             "definitions.rkt")
    (provide (rename-out
              [immutable-FS?     u:FS?]
              [make-immutable-FS u:make-FS]
              [dict-has-key?     u:FS-has-key?]
              [dict-ref          u:FS-ref]
              [dict-keys         u:FS-keys]))

    ;; TODO: put this in definition.rkt
    ;;
    ;; FS key equality:
    ;;
    ;; (key=? #'(a . b) #'(a . b))  ; #t
    ;; (key=? #'(a . b) #'(c . d))  ; #f
    ;; (key=? #'(a . 1) #'(a . 1))  ; #f
    (define (key=? key1-stx key2-stx)
      (match-let ([(cons C-TYPE1 F-NAME1) (syntax-e key1-stx)]
                  [(cons C-TYPE2 F-NAME2) (syntax-e key2-stx)])
        (and (bound-id=? C-TYPE1 C-TYPE2)
             (bound-id=? F-NAME1 F-NAME2))))

    (define-custom-hash-types FS key=?))

  (define-type FS-key (Syntaxof (Pairof B-TYPE Identifier)))

  (unsafe-require/typed 'untyped-FS
    [#:opaque FS u:FS?]
    [u:make-FS     ((Listof (Pairof FS-key OW-SCHEME)) -> FS)]
    [u:FS-has-key? (FS FS-key -> Boolean)]
    [u:FS-ref      (FS FS-key -> OW-SCHEME)]
    [u:FS-keys     (FS -> (Listof FS-key))])

  ;; Make FS
  (: make-FS ((Listof (Pairof FS-key OW-SCHEME)) -> FS))
  (define make-FS u:make-FS)

  ;; Is `Class-type . Field-name` member of `FS`.
  (: FS-member? (FS FS-key -> Boolean))
  (define FS-member? u:FS-has-key?)

  ;; Returns the `TYPE` of `Field-name` in `Class-type`.
  (: FS-ref (FS FS-key -> OW-SCHEME))
  (define FS-ref u:FS-ref)

  ;; List FS keys
  (: FS-keys (FS -> (Listof FS-key)))
  (define FS-keys u:FS-keys)

  (module+ test
    (provide (all-defined-out))

    (define FS (make-FS (list (cons #'(class . field) #'(t Θ (ν μ)))
                              (cons #'(clazz . dleif) #'(u o (n m))))))

    (define FS-tests
      (test-suite "Test for FS env"
       (check-true  (FS-member? FS #'(class . field)))
       (check-true  (FS-member? FS #'(clazz . dleif)))
       (check-false (FS-member? FS #'(Class . field)))
       (check-false (FS-member? FS #'(class . Field)))

       (check-stx=? (FS-ref FS #'(class . field)) #'(t Θ (ν μ)))
       (check-stx=? (FS-ref FS #'(clazz . dleif)) #'(u o (n m))))
      ))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; DS is the map of definitions, with the syntax #'(Class-type .
  ;; Def-name) as key and the def args type and def return type as
  ;; value.
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ;; Untyped world
  (module untyped-DS racket/base
    (require racket/dict racket/match
             "definitions.rkt")
    (provide (rename-out
              [immutable-DS?     u:DS?]
              [make-immutable-DS u:make-DS]
              [dict-has-key?     u:DS-has-key?]
              [dict-ref          u:DS-ref]))

    ;; Two DS-key are equals iff they are defined in the same class
    ;; and have the same name.
    (define (ds-key=? key1-stx key2-stx)
      (match-let ([(cons C-TYPE1 D-NAME1) (syntax-e key1-stx)]
                  [(cons C-TYPE2 D-NAME2) (syntax-e key2-stx)])
        (and
         (bound-id=? C-TYPE1 C-TYPE2)
         (bound-id=? D-NAME1 D-NAME2))))

    (define-custom-hash-types DS ds-key=?))

  (define-type DS-key (Syntaxof
                       (Pairof Identifier                     ; Class type
                               Identifier)))                  ; Def name
  (define-type DS-val (Syntaxof
                       (List (Syntaxof (Listof OW-SCHEME))  ; Def arg types
                             OW-SCHEME)))                   ; Def return type

  (unsafe-require/typed 'untyped-DS
    [#:opaque DS u:DS?]
    [u:make-DS     ((Listof (Pairof DS-key DS-val)) -> DS)]
    [u:DS-has-key? (DS DS-key -> Boolean)]
    [u:DS-ref      (DS DS-key -> DS-val)])

  ;; Make DS
  (: make-DS ((Listof (Pairof DS-key DS-val)) -> DS))
  (define make-DS u:make-DS)

  ;; Is `#'(Class-type . Def-name)` member of `DS`
  (: DS-member? (DS DS-key -> Boolean))
  (define DS-member? u:DS-has-key?)

  ;; Returns the args and return ownership types of Def-name in
  ;; Class-type
  (: DS-ref (DS DS-key -> DS-val))
  (define DS-ref u:DS-ref)

  (module+ test
    (provide (all-defined-out))

    (define DS (make-DS (list (cons #'(class . def1) #'(((Foo o ())) (Bar o (n m))))
                              (cons #'(clazz . def1) #'(((Foo u ())) (Bar v (w x))))
                              (cons #'(class . def0) #'(() (Bar v (w x)))))))

    (define DS-tests
      (test-suite "Test for DS env"
       (check-true  (DS-member? DS #'(class . def1)))
       (check-true  (DS-member? DS #'(clazz . def1)))
       (check-true  (DS-member? DS #'(class . def0)))
       (check-false (DS-member? DS #'(Class . def1)))
       (check-false (DS-member? DS #'(class . Def1)))

       (check-stx=? (DS-ref DS #'(class . def1)) #'(((Foo o ())) (Bar o (n m))))
       (check-stx=? (DS-ref DS #'(clazz . def1)) #'(((Foo u ())) (Bar v (w x))))
       (check-stx=? (DS-ref DS #'(class . def0)) #'(() (Bar v (w x))))
       )))
  )
