#lang typed/racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;;
;; Common definitions for parsers of the lang.

(require (for-syntax racket/base)
         ;; racket/dict
         racket/function
         racket/list
         racket/match
         syntax/parse
         ;; syntax/srcloc
         ;; syntax/parse/define
         ;; dbg macro.
         ;; TODO: remove this
         ;; (for-syntax racket/base
         ;;             racket/pretty
         ;;             racket/port)
         ;; END TODO
         typed/racket/unsafe
         "_definitions.rkt"
         )


;; (define-type PairofId (Pairof Identifier Identifier))

;; (require/typed syntax/parse
;;   ;; Coerce `flatten` to only return Listof String (instead of Listof
;;   ;; Any). This helps type-checking the `unlines` function.
;;   [syntax-parser (Any -> Any)])
(require/typed "utils.rkt"
  [check-stx=? (->* (Syntax Syntax) (String) Any)]
  [zip (All (a b) ((Listof a) (Listof b) -> (Listof (Pairof a b))))]
  [unzip (All (a b) ((Listof (Pairof a b)) -> (Values (Listof a) (Listof b))))])
(require/typed racket/list
  ;; (index-of keys key ids-eq?)
  [index-of (All (a) ((Listof a) a (a a -> Boolean) -> (U Nonnegative-Integer #f)))])

(provide (except-out (all-defined-out)
                     keyword-lits expr-lits
                     private:with-CS with-CS
                     private:map-index-of
                     ;; FIXME: make them private and define a proper
                     ;; interface to define them in order to ensure
                     ;; type safety with contracts.
                     private:CS private:FS
                     )
         (all-from-out "_definitions.rkt"))

;; Provide without the generation of contracts
;;
;; Macros defined in typed modules may not be used in untyped modules.
;; A workaround for such macros is provided them with `unsafe-provide`
;; which exports the macro without any contracts generated. See,
;; https://docs.racket-lang.org/ts-guide/typed-untyped-interaction.html#%28part._.Using_.Typed_.Code_in_.Untyped_.Code%29
;; https://groups.google.com/d/msg/racket-users/eowl6RpdDwY/1wrCluDcAwAJ
(unsafe-provide keyword-lits expr-lits
                private:CS private:FS
                with-CS)


;; Language definitions

(define-literal-set keyword-lits
  #:for-label
  ;; Note: The syntax/parse package require all literals to have a
  ;; binding. To match identifier by their symbolic names, I have to
  ;; use `#:datum-literals` instead.
  #:datum-literals (prog class field def)
  ;; I have no literals that should be interpreted.
  ())

(define-literal-set expr-lits
  #:for-label
  #:datum-literals (let new send get-field set-field! ???)
  ())

(define-type OW-SCHEME   (Syntaxof
                          (List Identifier                         ;; Basic Type
                                Identifier                         ;; Owner
                                (Syntaxof (Listof Identifier)))))  ;; C-Params


;; > (ow-scheme? #'(a b ()))       ; #t
;; > (ow-scheme? #'(a b [c d e]))  ; #t
;; > (ow-scheme? #'(a 1 []))       ; #f
(define-predicate ow-scheme? OW-SCHEME)

;; > (ow-scheme-eq? #'(a b ()) #'(a b ()))            ; #t
;; > (ow-scheme-eq? #'(a b [c d e]) #'(a b [c d e]))  ; #t
;; > (ow-scheme-eq? #'(a b ()) #'(c b ()))            ; #f
;; > (ow-scheme-eq? #'(a b [c d e]) #'(a b [e d c]))  ; #f
(: ow-scheme-eq? (OW-SCHEME OW-SCHEME -> Boolean))
(define (ow-scheme-eq? ow1 ow2)
  (match-let ([(list TYPE1 OWNER1 CPARAMS1) (syntax-e ow1)]
              [(list TYPE2 OWNER2 CPARAMS2) (syntax-e ow2)])
    (and (bound-id=? TYPE1 TYPE2)
         ;; FIXME: I have to instantiate the scheme to know if they
         ;; are eq?
         #;(bound-id=? OWNER1 OWNER2)
         #;(for/and ([id1 (syntax->list CPARAMS1)]
                   [id2 (syntax->list CPARAMS2)])
             (bound-id=? id1 id2))
         (eq? (length (syntax->list CPARAMS1))
              (length (syntax->list CPARAMS2)))
         )))

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


;; (define (make-ow-scheme OW-SCHEME OWNER CPARAMS #:stx-src [stx-src #f])
;;   (define ow-scheme-stx #`(ow-scheme #,OW-SCHEME #,OWNER #,CPARAMS))

;;   (if stx-src
;;     (quasisyntax/loc stx-src #,ow-scheme-stx)
;;     ow-scheme-stx))

;; ;; Returns `#t` if the syntax object is a def.
;; ;; def? : Syntax -> Boolean
;; (define def?
;;   (syntax-parser
;;     ;; #:literal-sets [expr-lits]
;;     [(def _ ...) #t]
;;     [_ #f]))


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


;; Get or set the type of a syntax object.
(: type-prop
   (All (a) (case->
             [(Syntaxof a) -> (U OW-SCHEME #f)]
             [(Syntaxof a) OW-SCHEME -> (Syntaxof a)])))
(define type-prop
  (case-lambda
    ;; Get the type of a syntax object
    [(stx) (let ([τ (syntax-property stx 'type)])
             (if (ow-scheme? τ) τ #f))]
    ;; Set the type of a syntax object
    [(stx OW-SCHEME) (syntax-property stx 'type OW-SCHEME)]))


;; Environments

;;~~~~~~~~~~~~~~~~~
;; CS: Set of Types

(: private:CS (Parameterof (Listof Identifier)))
(define private:CS (make-parameter '()))

;; Is Identifier exists in CS?
(: CS-member? (Identifier ->  Boolean))
(define (CS-member? id-stx)
  (if (findf (curry bound-id=? id-stx) (private:CS)) #t #f))

;; Make `the-CS` the value of `CS` in `A`.
(: private:with-CS
   (All (A) (U (Syntaxof (Listof Identifier)) (Listof Identifier)) (-> A) -> A))
(define (private:with-CS the-CS thunk-E)
  (parameterize
      ([private:CS
        (cond
          [(and (syntax? the-CS) (syntax->list the-CS)) => identity]
          [else the-CS])])
    ;; (dbg (CS))
    (thunk-E)))

;; Automatically create the `thunk` around E expressions
(define-syntax (with-CS stx)
  (syntax-case stx ()
    [(_ THE-CS E ...) #'(private:with-CS THE-CS (thunk E ...))]))

(module+ test
  (require typed/rackunit)

  (with-CS #'(foo bar)
    (check-true  (CS-member? #'foo))
    (check-false (CS-member? #'baz)))

  (with-CS (list #'foo #'bar)
    (check-true  (CS-member? #'foo))
    (check-false (CS-member? #'baz))))

;;~~~~~~~~~~~~~~~~~~
;; FS: Map of fields
;;
;; With the syntax #'(Class-type . Field-name) as key and the Field
;; type as value.
(define-type FS-key (Syntaxof (Pairof Identifier Identifier)))
(define-type FS (Map FS-key OW-SCHEME))
(: private:FS (Parameterof FS))
(define private:FS (make-parameter '()))

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

(: FS-member? (FS-key -> Boolean))
(define (FS-member? C-TYPE.FIELD)
  (map-has-key? (private:FS) C-TYPE.FIELD fs-key=?))

(: FS-set (FS-key OW-SCHEME -> FS))
(define (FS-set C-TYPE.FIELD OW-SCHEME)
  (map-set (private:FS) C-TYPE.FIELD OW-SCHEME fs-key=?))

(: FS-ref (FS-key -> OW-SCHEME))
(define (FS-ref C-TYPE.FIELD)
  (map-ref (private:FS) C-TYPE.FIELD fs-key=?))

(module+ test
  (require typed/rackunit)

  (define test:τFoo #'(foo-type rep ()))
  (define test:τBar #'(bar-type rep ()))

  (: test:FS FS)
  (define test:FS
    `((,#'(c . foo) . ,test:τFoo)
      (,#'(c . bar) . ,test:τBar)))

  (: stx-eq? (OW-SCHEME OW-SCHEME -> Boolean))
  (define (stx-eq? a b)
    (equal? (syntax->datum a) (syntax->datum b)))

  (parameterize ([private:FS test:FS])
    (check-true  (FS-member? #'(c . foo)))
    (check-false (FS-member? #'(C . foo)))
    (check-false (FS-member? #'(c . Foo)))
    (check-false (FS-member? #'(c . baz)))

    (check-stx=? (FS-ref #'(c . foo)) test:τFoo)
    (check-stx=? (FS-ref #'(c . bar)) test:τBar)

    (check-true  (map-eq? test:FS (FS-set #'(c . foo) test:τFoo) fs-key=? stx-eq?))
    (check-false (map-eq? test:FS (FS-set #'(C . foo) test:τFoo) fs-key=? stx-eq?))
    (check-false (map-eq? test:FS (FS-set #'(c . Foo) test:τFoo) fs-key=? stx-eq?))
    (check-false (map-eq? test:FS (FS-set #'(c . foo) test:τBar) fs-key=? stx-eq?))
    (check-false (map-eq? test:FS (FS-set #'(c . baz) test:τBar) fs-key=? stx-eq?))))

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
(define-type DS (Map DS-key OW-SCHEME))
(: private:DS (Parameterof DS))
(define private:DS (make-parameter '()))

;; (ds-key=? #'(a b ()) #'(a b ()))                  ; #t
;; (ds-key=? #'(a b [(c d ())]) #'(a b [(c d ())]))  ; #t
;; (ds-key=? #'(a b ()) #'(c d ()))                  ; #f
;; (ds-key=? #'(a b [(c d ())]) #'(a b [(d c ())]))  ; #f
(: ds-key=? (DS-key DS-key -> Boolean))
(define (ds-key=? key1-stx key2-stx)
  (match-let ([(list C-TYPE1 D-NAME1 ARGs-OW-SCHEME1) (syntax-e key1-stx)]
              [(list C-TYPE2 D-NAME2 ARGs-OW-SCHEME2) (syntax-e key2-stx)])
    (and
     (bound-id=? C-TYPE1 C-TYPE2)
     (bound-id=? D-NAME1 D-NAME2)
     (for/and ([ow1 (syntax->list ARGs-OW-SCHEME1)]
               [ow2 (syntax->list ARGs-OW-SCHEME2)])
       (ow-scheme-eq? ow1 ow2)))))

(: DS-member? (DS-key -> Boolean))
(define (DS-member? ds-key)
  (map-has-key? (private:DS) ds-key ds-key=?))

(: DS-set (DS-key OW-SCHEME -> DS))
(define (DS-set ds-key OW-SCHEME)
  (map-set (private:DS) ds-key OW-SCHEME ds-key=?))

(: DS-ref (DS-key -> OW-SCHEME))
(define (DS-ref ds-key)
  (map-ref (private:DS) ds-key ds-key=?))


;; Utils

(: bound-id=? (Identifier Identifier -> Boolean))
(define (bound-id=? id1 id2)
  (eq? (syntax-e id1) (syntax-e id2)))

;; A typable map with custom function for comparing keys
(define-type (Map a b) (Listof (Pairof a b)))

(: map-ref (All (a b) ((Map a b) a (a a -> Boolean) -> b)))
(define (map-ref the-map key key-eq?)
  (cond
    [(assoc key the-map key-eq?) => cdr]
    [else (error "the key ~s does not exist in Map" key)]))

(: map-set (All (a b) ((Map a b) a b (a a -> Boolean) -> (Map a b))))
(define (map-set the-map key val key-eq?)
  (cond
    [(private:map-index-of the-map key key-eq?)
     => (λ (idx) (list-set the-map idx (cons key val)))]
    [else (cons (cons key val) the-map)]))

(: private:map-index-of
   (All (a b) ((Map a b) a (a a -> Boolean) -> (U Nonnegative-Integer #f))))
(define (private:map-index-of the-map key key-eq?)
  (let-values ([(keys _) (unzip the-map)])
    (index-of keys key key-eq?)))

(: map-has-key? (All (a b) ((Map a b) a (a a -> Boolean) -> Boolean)))
(define (map-has-key? the-map key key-eq?)
  (and (private:map-index-of the-map key key-eq?) #t))

(: map-eq?
   (All (a b) ((Map a b) (Map a b) (a a -> Boolean) (b b -> Boolean) -> Boolean)))
(define (map-eq? map1 map2 k-eq? v-eq?)
  (for/and ([kv1 map1]
            [kv2 map2])
    (let ([k1 (car kv1)] [v1 (cdr kv1)]
          [k2 (car kv2)] [v2 (cdr kv2)])
      (and (k-eq? k1 k2) (v-eq? v1 v2)))))
