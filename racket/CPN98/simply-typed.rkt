#lang typed/racket/base/no-check

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.
;;
;; Simple type checking phase (?>)
;; - Type checks the program (for simple type -- "simple" as in simply
;;   typed λ calculus, i.e., no ownership).
;; - Based on [FKF98] (see Bibliography).
;;
;; Environments:
;; - Γ is the map of locally bound variables.  Used to track free
;;   variables.
;; - τ is the type of the current class.  Used to get the type of
;;   `this`.
;; - CS is the set of defined types.  A type which does not exist in
;;   CS raises an `unknown-type` error.
;; - FS is the map of fields.  The map has the class name and field
;;   name as value, and the field type as key.  Used to:
;;   + Ensure that a field is a member of specific class (raise an
;;     `unknown-field` error otherwise).
;;   + Ensure that an expression that is going to be set into a
;;     specific field is of the correct type (raise an `unknown-type`
;;     error otherwise).
;;   + Get the type of a `get-field` operation.
;; - DS is the map of definitions.  The map has the class name, def
;;   name and def arguments type as values, and the return type as
;;   key.  Used to:
;;   + Ensure that a def is a member of specific class (raise an
;;     `unknown-def` error otherwise).
;;   + Ensure that a call of a def has the expected number of
;;     arguments (raise an `arity-error` otherwise).
;;   + Ensure that a call of a def has arguments of the expected type
;;     (raise an `unknown-type` error otherwise).
;;   + Get the type of a `send` operation.
;;
;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects
;;
;; Global:
;; - meta:CS is the set of defined ownership scheme
;; - meta:FS is the map of fields with ownership scheme field as value
;; - meta:DS is the map of definitions with return ownership sheme as
;;   value

(require (for-syntax racket/base)
         racket/dict
         racket/function
         racket/list
         racket/match
         racket/syntax
         syntax/parse
         syntax/parse/define
         syntax/srcloc
         syntax/stx
         "utils.rkt"
         "meta.rkt"
         (prefix-in env: (submod "env.rkt" basic-check))
         (rename-in "definitions.rkt"
                    [dict-ref      def/dict-ref]
                    [dict-has-key? def/dict-has-key?]
                    [dict-map      def/dict-map]
                    [dict-keys     def/dict-keys]))

(module+ test (require rackunit))

(provide ?>)


;; Phase ?>

(define-parser (?> stx)
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; TODO: Find a better syntax. Maybe something à la datatype?
  ;; [Γ : Id~>B-TYPE
  ;;    #:mk   env:make-Γ
  ;;    #:init '()
  ;;    #:partial-app ([env:Γ-member? Γ-member?]
  ;;                   [env:Γ-add     Γ-add]
  ;;                   [env:Γ-ref     Γ-ref])]
  #:Env
  (;; Map of locally bound variables.
   [Γ : (HashTable Identifier B-TYPE)
      env:make-Γ '()
      #:partial-app ([env:Γ-member? Γ-member?]
                     [env:Γ-add     Γ-add]
                     [env:Γ-ref     Γ-ref])]

   ;; Store of the current class type
   [τ : B-TYPE
      #'Top]

   ;; Set of existing types
   [CS : (Setof B-TYPE)
       env:make-CS meta:CS
       #:partial-app ([env:CS-member? CS-member?])]

   ;; Map of fields
   [FS : (HashTable
          (Syntaxof (Pairof B-TYPE Identifier)) ;; #'(Class-type . Field-name)
          B-TYPE)                               ;; #'Field-type
       env:make-FS
       (meta-map  ;; Instantiate ows in values
        (syntax-parser [SCHEME:ow-scheme #'SCHEME.TYPE])
        meta:FS)
       #:partial-app ([env:FS-member? FS-member?]
                      [env:FS-ref FS-ref])]

   ;; Map of definitions
   [DS : (HashTable
          (Syntaxof (List Identifier                    ; Class type
                          Identifier                    ; Def name
                          (Syntaxof (Listof B-TYPE))))  ; Type of def args
          B-TYPE)                                       ; Def return type
       env:make-DS
       (meta-map-kv
        ;; Instantiate ows in keys
        (syntax-parser [(c-type:id def:id (scheme:ow-scheme ...))
                        #:with b-types #'(scheme.TYPE ...)
                        #'(c-type def b-types)])
        ;; Instantiate ows in values
        (syntax-parser [SCHEME:ow-scheme #'SCHEME.TYPE])
        meta:DS)
       #:partial-app ([env:DS-member? DS-member?]
                      [env:DS-ref DS-ref]
                      [env:DS-domain DS-domain])])

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Parse
  (⊢p stx))


;; ⊢p P ≫ ?P : t
;;
;; `P` elaborates to `?P` and has type `t`
(define-rules ⊢p
  ;; [prog]
  [(prog ~! CLASS ... E)
   ;; Check P ⊢d CLASS ≫ ?CLASS
   #:with [?CLASS ...] (stx-map ⊢d #'(CLASS ...))
   ;; Check P,[] ⊢e E ≫ ?E : t
   #:with [?E t] (get-τ (with-Γ #'() (⊢e #'E)))
   ;; ----------------------------------------------------------------
   ;; ⊢p (prog CLASS ... E) ≫ (prog ?CLASS ... ?E) : t
   (add-τ this-syntax #'t)])


;; P ⊢d CLASS ≫ ?CLASS
;;
;; In context `P`, `CLASS` elaborates to `?CLASS`
(define-rules ⊢d
  ;; [defn]
  [(class ~! NAME [CPARAM ...] FIELD/DEF ...)
   #:with [FIELD ...] (filter field? (stx->list #'(FIELD/DEF ...)))
   #:with [DEF ...] (filter def? (stx->list #'(FIELD/DEF ...)))
   ;; Check P ⊢τ t on fields
   #:with [(field ~! F-NAME F:ow-scheme) ...] #'(FIELD ...)
   #:when (stx-for/and ([T #'(F.TYPE ...)]) (⊢τ T))
   ;; Check P,NAME ⊢m DEF ≫ ?DEF
   #:with [?DEF ...] (with-τ #'NAME (stx-map ⊢m #'(DEF ...)))
   ;; ----------------------------------------------------------------
   ;; P ⊢d (class NAME FIELD ... DEF ...) ≫ (class NAME FIELD ... ?DEF ...)
   ;; #@(class NAME [CPARAM ...] FIELD ... ?DEF ...)]
   this-syntax])


;; P,τ ⊢m DEF ≫ ?DEF
;;
;; In context `P,τ`, `DEF` elaborates to `?DEF`
(define-rules ⊢m
  ;; [meth]
  [(def ~! (NAME (ARG-NAME ARG:ow-scheme) ... RET:ow-scheme) E)
   ;; Get current class type store in τ environment
   #:with τ0 (τ)
   ;; Check P ⊢τ t on args and return type
   #:when (stx-for/and ([T #'(ARG.TYPE ... RET.TYPE)]) (⊢τ T))
   ;; Check P,{this: τ0, ARG-NAME: ARG-TYPE, ...} ⊢e E ≫ ?E : RET-TYPE
   #:with [?E t-e] (get-τ
                    (with-Γ #'{ (this     . τ0)
                                (???      . RET.TYPE)
                                (ARG-NAME . ARG.TYPE) ... }
                      (⊢e  #'E)))
   #:when (or (τ=? #'t-e #'RET.TYPE)
              (raise-type-mismatch #'t-e #'RET.TYPE #'?E))
   ;; ----------------------------------------------------------------
   ;; P,τ0 ⊢m (def (NAME (ARG-NAME ARG-OW-SCHEME) ... RET-OW-SCHEME) E) ≫
   ;;           (def (NAME (ARG-NAME ARG-OW-SCHEME) ... RET-OW-SCHEME) ?E)
   this-syntax])

(module+ test
  (define-test-suite ⊢m-parse
    (with-Γ #'{ (_ . Bar) }
    (with-τ #'Foo
    (with-CS (list #'Foo #'Bar)

      ;; [meth] (def ~! (NAME (ARG-NAME ARG:ow-scheme) ... RET:ow-scheme) E)
      ;; Check P ⊢τ t on args and return type
      (check-exn exn:unknown-type?
                 (thunk (⊢m #'(def (def/2 (arg1 (Baz o ())) (arg2 (Foo o ())) (Bar o ())) _)))
                 "`Baz` is not a defined type")
      (check-exn exn:unknown-type?
                 (thunk (⊢m #'(def (def/2 (arg1 (Bar o ())) (arg2 (Baz o ())) (Bar o ())) _)))
                 "`Baz` is not a defined type")
      (check-exn exn:unknown-type?
                 (thunk (⊢m #'(def (def/2 (arg1 (Bar o ())) (arg2 (Foo o ())) (Baz o ())) _)))
                 "`Baz` is not a defined type")
      ;; Check P,{this: τ0, ARG-NAME: ARG-TYPE, ...} ⊢e E ≫ ?E : RET-TYPE
      (check-exn exn:type-mismatch?
                 (thunk (⊢m #'(def (def/2 (arg1 (Bar o ())) (arg2 (Foo o ())) (Bar o ()))
                                (new (Foo o ())))))
                 "`def/2` should return a `Bar` but the expression is of type `Foo`")
      (check-not-exn
       (thunk (⊢m #'(def (def/0 (Foo o ())) this)))
       "`this` is bound in the expression with the `Foo` type (τ env)")
      (check-not-exn
       (thunk (⊢m #'(def (def/0 (Foo o ())) ???)))
       "A def accepts the `???` place holder as expression" )
      (check-not-exn
       (thunk (⊢m #'(def (def/0 (Bar o ())) ???)))
       "A def accepts the `???` place holder as expression" )
      (check-not-exn
       (thunk (⊢m #'(def (def/2 (arg1 (Bar o ())) (arg2 (Foo o ())) (Bar o ())) arg1)))
       "`arg1` is bound in the expression with the `Bar` type")
      (check-not-exn
       (thunk (⊢m #'(def (def/2 (arg1 (Bar o ())) (arg2 (Foo o ())) (Foo o ())) arg2)))
       "`arg2` is bound in the expression with the `Foo` type")
      )))))

;; P,Γ ⊢e E ≫ ?E : t
;;
;; In context `P,Γ`, `E` elaborates to `?E` and has type `t`
(define-rules ⊢e
  ;; [new]
  [(new ~! SCHEME:ow-scheme)
   ;; Check P ⊢τ SCHEME-TYPE
   #:when (⊢τ #'SCHEME.TYPE)
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e (new C) ≫ (new C) : C
   (add-τ this-syntax #'SCHEME.TYPE)]

  ;; [var]
  [ID:id
   ;; Check ID ∈ dom(Γ)
   #:when (or (Γ-member? #'ID)
              ;; Unbound identifier. This is not supposed to happened
              ;; thanks to desugaring, but who knows ...
              (raise-syntax-error #f "unbound identifier" #'ID))
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e ID ≫ ID : Γ(ID)
   (add-τ this-syntax (Γ-ref #'ID))]

  ;; [get]
  [(get-field ~! E FNAME)
   ;; Check P,Γ ⊢e E ≫ ?E : t
   #:with [?E t] (get-τ (⊢e #'E))
   ;; Check (t . FNAME) ∈ dom(FS)
   ;;;; The field FNAME is defined in the class t
   #:when (or (FS-member? #'(t . FNAME)) (raise-unknown-field #'FNAME #'t))
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e (get-field E FNAME) ≫
   ;;          (get-field (?E : t) FNAME) : FS(t . FNAME)
   (add-τ this-syntax (FS-ref #'(t . FNAME)))]

  ;; [set]
  [(set-field! ~! E FNAME BODY)
   ;; Check P,Γ ⊢e E ≫ ?E : t
   #:with [?E t] (get-τ (⊢e #'E))
   ;; Check (t . FNAME) ∈ dom(FS)
   ;;;; The field FNAME is defined in the class t
   #:when (or (FS-member? #'(t . FNAME)) (raise-unknown-field #'FNAME #'t))
   ;; Check P,Γ ⊢e BODY ≫ ?BODY : FS(t . FNAME)
   ;;;; The BODY has to elaborate to something that fit into the
   ;;;; field.
   #:with [?BODY t-body] (get-τ (⊢e #'BODY))
   #:with t-field (FS-ref #'(t . FNAME))
   #:when (or (τ=? #'t-body #'t-field)
              (raise-type-mismatch #'t-body #'t-field))
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e (set-field E FNAME BODY) ≫
   ;;          (set-field (?E : t) FNAME ?BODY) : FS(t . FNAME)
   (add-τ this-syntax #'t-field)]

  ;; [call]
  [(send ~! E DNAME PARAM ...)
   ;; Check P,Γ ⊢e E ≫ ?E : t
   #:with [?E t] (get-τ (⊢e #'E))
   ;; Check P,Γ ⊢e PARAM ... ≫ (?PARAM : t-param) ...
   #:with [(?PARAM t-param) ...] (stx-map (∘ get-τ ⊢e) #'(PARAM ...))
   ;; Check (t DNAME (t-param ...)) ∈ dom(DS)
   ;;;; The method DNAME with parameters (t-param ...) is defined in
   ;;;; the class t.
   #:with DS-key #'(t DNAME (t-param ...))
   #:when (or (DS-member? #'DS-key) (raise-def-error #'DS-key))
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e (send E DNAME PARAM ...) ≫
   ;;          (send (?E : t) DNAME ?PARAM ...) : DS(t DNAME t-param ...)
   (add-τ this-syntax (DS-ref #'DS-key))]

  ;; [let]
  [(let ~! (VAR-NAME VAR-SCHEME:ow-scheme E) BODY ...)
   ;; Check P ⊢τ VAR-SCHEME-TYPE
   #:when (⊢τ #'VAR-SCHEME.TYPE)
   ;; Check  P,Γ ⊢e E ≫ ?E : VAR-SCHEME-TYPE
   #:with [?E t] (get-τ (with-Γ (Γ-add #'(??? . VAR-SCHEME.TYPE))
                          (⊢e #'E)))
   #:when (or (τ=? #'t #'VAR-SCHEME.TYPE)
              (raise-type-mismatch #'t #'VAR-SCHEME.TYPE #'?E))
   ;; Check P,Γ{VAR-NAME: VAR-OW-SCHEME} ⊢e BODY ≫ ?BODY : t
   ;;;; Note: This is a generalization regarding [FKF98].  The paper
   ;;;; only accept one expression in the body of a `let`.  Here we
   ;;;; accept many, with the idea that the results of the last
   ;;;; expression is returned as the result of the `let`.  In such a
   ;;;; case the type of the `let` is the type of the Last Expression
   ;;;; of its BODY (LEB).
   ;;;; Check P,Γ{VAR-NAME: VAR-OW-SCHEME} ⊢e BODY ... LEB ≫ ?BODY ... ?LEB : t-body
   #:with [?BODY ... ?LEB] (with-Γ (Γ-add #'(VAR-NAME . VAR-SCHEME.TYPE))
                             (stx-map ⊢e #'(BODY ...)))
   #:with [_ t-body] (get-τ #'?LEB)
   ;; ------------------------------------------------------------------
   ;; P,Γ ⊢e *let (VAR-NAME VAR-OW-SCHEME E) BODY ... LEB ≫
   ;;           *let (VAR-NAME VAR-OW-SCHEME ?E) ?BODY ... ?LEB : t-body
   (add-τ this-syntax #'t-body)])

(module+ test
  (define-test-suite ⊢e-parse
    (with-Γ #'{ (this . Foo) (_ . Bar) }
    (with-CS (list #'Foo #'Bar)
    (with-FS (list (cons #'(Foo . bar) #'Bar))
    (with-DS (list (cons #'(Foo def/0 ())  #'Bar)
                   (cons #'(Foo def/2 (Foo Bar)) #'Bar))

      ;; [new]
      ;;;; Check P ⊢τ VAR-OW-SCHEME
      (check-exn exn:unknown-type? (thunk (⊢e #'(new (Baz o ())))))
      (check-not-exn (thunk (⊢e #'(new (Foo o ())))))
      ;;;; P,Γ ⊢e (new C) ≫ (new C) : C
      (check-τ (⊢e #'(new (Foo o ()))) #'Foo)
      (check-τ (⊢e #'(new (Foo p ()))) #'Foo)
      (check-τ (⊢e #'(new (Foo p (c)))) #'Foo)

      ;; [var]
      ;;;; Check ID ∈ dom(Γ)
      (check-not-exn (thunk (⊢e #'this)))
      (check-not-exn (thunk (⊢e #'_)))
      (check-exn exn:fail:syntax? (thunk (⊢e #'baz)))
      ;;;; P,Γ ⊢e ID ≫ ID : Γ(ID)
      (check-τ (⊢e #'this) #'Foo)
      (check-τ (⊢e #'_)  #'Bar)

      ;; [get]
      ;;;; Check P,Γ ⊢e E ≫ ?E : t
      ;;;; Check (t . FNAME) ∈ dom(FS): the field FNAME is defined in
      ;;;; the class t
      (check-exn exn:unknown-field? (thunk (⊢e #'(get-field (new (Bar o ())) bar))))
      (check-exn exn:unknown-field? (thunk (⊢e #'(get-field (new (Foo o ())) g))))
      (check-exn exn:unknown-field? (thunk (⊢e #'(get-field _ bar))))
      (check-not-exn (thunk (⊢e #'(get-field this bar))))
      (check-not-exn (thunk (⊢e #'(get-field (new (Foo o ())) bar))))
      (check-not-exn (thunk (⊢e #'(get-field (new (Foo p ())) bar))))   ;; <| Only look at
      (check-not-exn (thunk (⊢e #'(get-field (new (Foo p (c))) bar))))  ;;  | basic type.
      ;;;; P,Γ ⊢e (get-field E FNAME) ≫
      ;;;;          (get-field (?E : t) FNAME) : FS(t . FNAME)
      (check-τ (⊢e #'(get-field (new (Foo o ())) bar)) #'Bar)
      (check-τ (⊢e #'(get-field this bar)) #'Bar)

      ;; [set]
      ;;;; Check P,Γ ⊢e E ≫ ?E : t
      ;;;; Check (t . FNAME) ∈ dom(FS): the field FNAME is defined in
      ;;;; the class ?t
      (check-exn exn:unknown-field?
                 (thunk (⊢e #'(set-field! (new (Foo o ())) g _))))
      (check-exn exn:unknown-field?
                 (thunk (⊢e #'(set-field! (new (Bar o ())) bar _))))
      (check-exn exn:unknown-field?
                 (thunk (⊢e #'(set-field! _ bar _))))
      ;;;; Check P,Γ ⊢e BODY ≫ ?BODY : FS(t . FNAME): the BODY has to
      ;;;; elaborate to something that fit into the field.
      (check-exn exn:type-mismatch?
                 (thunk (⊢e #'(set-field! (new (Foo o ())) bar (new (Foo o ()))))))
      (check-exn exn:type-mismatch?
                 (thunk (⊢e #'(set-field! (new (Foo o ())) bar this))))
      (check-not-exn
       (thunk (⊢e #'(set-field! (new (Foo o ())) bar _))))
      (check-not-exn
       (thunk (⊢e #'(set-field! (new (Foo o ())) bar (new (Bar o ()))))))
      (check-not-exn                                                       ;; <| Only look at
       (thunk (⊢e #'(set-field! (new (Foo o ())) bar (new (Bar p ()))))))  ;;  | basic type,
      (check-not-exn                                                       ;;  | not owner
       (thunk (⊢e #'(set-field! (new (Foo o ())) bar (new (Bar p (c))))))) ;;  | nor ctx params
      ;;;; P,Γ ⊢e (set-field E FNAME BODY) ≫
      ;;;;          (set-field (?E : t) FNAME ?BODY) : FS(t . FNAME)
      (check-τ (⊢e #'(set-field! (new (Foo o ())) bar (new (Bar o ())))) #'Bar)
      (check-τ (⊢e #'(set-field! (new (Foo o ())) bar _)) #'Bar)
      ;; ;; TODO: implement me
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(set-field! (new (Foo o ())) bar ???)))
      ;;  "A `set-field!` accepts the `???` expression")
      ;; (check-τ (⊢e #'(set-field! (new (Foo o ())) bar ???)) #'Bar)

      ;; [call] (send ~! E DNAME PARAM ...)
      ;;;; Check P,Γ ⊢e E ≫ ?E : t
      ;;;; Check P,Γ ⊢e PARAM ... ≫ (?PARAM : t-param) ...
      ;;;; Check (t DNAME (t-param ...)) ∈ dom(DS): the method DNAME
      ;;;; with parameters (t-param ...) is defined in the class t.
      (check-exn exn:unknown-def?
                 (thunk (⊢e #'(send (new (Foo o ())) udef))))
      (check-exn exn:arity-error?
                 (thunk (⊢e #'(send (new (Foo o ())) def/0 _))))
      (check-exn exn:arity-error?
                 (thunk (⊢e #'(send (new (Foo o ())) def/0 _ _))))
      (check-exn exn:arity-error?
                 (thunk (⊢e #'(send (new (Foo o ())) def/2))))
      (check-exn exn:arity-error?
                 (thunk (⊢e #'(send (new (Foo o ())) def/2 _))))
      (check-exn exn:arity-error?
                 (thunk (⊢e #'(send (new (Foo o ())) def/2 _ _ _))))
      (check-exn exn:type-mismatch?
                 (thunk (⊢e #'(send (new (Foo o ())) def/2 _ _))))
      (check-exn exn:type-mismatch?
                 (thunk (⊢e #'(send (new (Foo o ())) def/2 this this))))
      (check-exn exn:type-mismatch?
                 (thunk (⊢e #'(send (new (Foo o ())) def/2 _ this))))
      (check-not-exn
       (thunk (⊢e #'(send (new (Foo o ())) def/0))))
      (check-not-exn
       (thunk (⊢e #'(send (new (Foo o ())) def/2 this _))))
      (check-not-exn
       (thunk (⊢e #'(send (new (Foo o ())) def/2 (new (Foo o ())) (new (Bar o ()))))))
      ;;;; Only look at basic type, not owner nor ctx params.
      (check-not-exn
       (thunk (⊢e #'(send (new (Foo o ())) def/2 (new (Foo p ())) (new (Bar p ()))))))
      (check-not-exn
       (thunk (⊢e #'(send (new (Foo o ())) def/2 (new (Foo p (c))) (new (Bar p (c)))))))
      ;;;; P,Γ ⊢e (send E DNAME PARAM ...) ≫
      ;;;;          (send (?E : t) DNAME ?PARAM ...) : DS(t DNAME PARAM ...)
      (check-τ (⊢e #'(send (new (Foo o ())) def/0)) #'Bar)
      (check-τ
       (⊢e #'(send (new (Foo o ())) def/2 (new (Foo o ())) (new (Bar o ()))))
       #'Bar)
      (check-τ (⊢e #'(send (new (Foo o ())) def/2 this _)) #'Bar)
      ;; ;; TODO: implement me
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(send (new (Foo o ())) def/2 ?arg1 ?arg2)))
      ;;  "A `send` accepts the `?_` expression")
      ;; (check-τ (⊢e #'(send (new (Foo o ())) def/2 ?arg1 ?arg2)) #'Bar)

      ;; [let] (let ~! (VAR-NAME VAR-SCHEME:ow-scheme E) BODY ...)
      ;;;; Check P ⊢τ VAR-SCHEME-TYPE
      (check-exn exn:unknown-type? (thunk (⊢e #'(let (baz (Baz o ()) _) _))))
      ;;;; Check  P,Γ ⊢e E ≫ ?E : VAR-OW-SCHEME
      (check-exn exn:type-mismatch? (thunk (⊢e #'(let (baz (Foo o ()) (new (Bar o ()))) _))))
      ;;;; Check P,Γ{VAR-NAME: VAR-OW-SCHEME} ⊢e BODY ... LEB ≫ ?BODY ... ?LEB : t-body
      (check-not-exn
       (thunk (⊢e #'(let (foo (Foo o ()) this) foo))))
      (check-not-exn
       (thunk (⊢e #'(let (foo (Foo o ()) this) _))))
      (check-not-exn
       (thunk (⊢e #'(let (foo (Foo o ()) this) _ _))))
      (check-not-exn
       (thunk (⊢e #'(let (foo (Foo o ()) ???) _)))
       "A let binding accepts the `???` as expression")
      (check-not-exn
       (thunk (⊢e #'(let (foo (Foo o ())  (new (Foo o ()))) _))))
      (check-not-exn                                               ;; <| Only look at
       (thunk (⊢e #'(let (foo (Foo p ())  (new (Foo o ()))) _))))  ;;  | basic type,
      (check-not-exn                                               ;;  | not owner
       (thunk (⊢e #'(let (foo (Foo p (c)) (new (Foo o ()))) _))))  ;;  | nor ctx params
      ;;;; P,Γ ⊢e *let (VAR-NAME VAR-OW-SCHEME E) BODY ... LEB ≫
      ;;;;           *let (VAR-NAME VAR-OW-SCHEME ?E) ?BODY ... ?LEB : t-body
      (check-τ (⊢e #'(let (foo (Foo o ()) ???) foo))   #'Foo)
      (check-τ (⊢e #'(let (foo (Foo o ()) ???) _))     #'Bar)
      (check-τ (⊢e #'(let (foo (Foo o ()) ???) foo _)) #'Bar)
      (check-τ (⊢e #'(let (foo (Foo o ()) ???) _ foo)) #'Foo)
      (check-τ (⊢e #'(let (foo (Foo o ()) ???) _ _))   #'Bar)
      (check-τ (⊢e #'(let (foo (Foo o ()) ???) (let (foo (Bar o ()) ???) foo)))
               #'Bar
               "A inner `let` shadows bindings of the outer `let`s")
      ))))))


;; P ⊢τ t
;;
;; In context `P`, `t` exists
;;
;; Note: I put this in a specific rules because checking `t` will get
;; more hairy if I eventually implement inheritance.
(define-rules ⊢τ
  ;; [type]
  [t #:when (or (CS-member? #'t) (raise-unknown-type #'t))
     this-syntax])

(module+ test
  (define-test-suite ⊢τ-parse
    (with-CS (list #'Foo #'Bar)
      (check-not-exn (thunk (⊢τ #'Foo)))
      (check-not-exn (thunk (⊢τ #'Bar)))
      (check-exn exn:unknown-type? (thunk (⊢τ #'Baz))))))


;; Environment

;; DS-key is not a member implies three possible causes:
;;;; I met no c-name=? and d-name=? => unknown definition
;;;; I met c-name=? and d-name=? but not num-args? => arity error
;;;; I met c-name=?, d-name=? and num-args? => type mismatch
(define (raise-def-error LOOKED-DS-key)
  ;; Get info of the looked def
  (match-define (list LOOKED-DEF-C-TYPE LOOKED-DEF-NAME _LOOKED-DEF-ARGs)
    (syntax-e LOOKED-DS-key))
  (define LOOKED-DEF-ARGs (syntax->list _LOOKED-DEF-ARGs))
  (define looked-def-arity (length LOOKED-DEF-ARGs))

  ;; Get the domain of `DS` and transform info for latter analysis
  ;; (: def-dom (Listof (List B-TYPE Identifier (Listof B-TYPE))))
  (define def-dom
    (map (∘ (λ (key)
              (match-define (list c-type def-name args) key)
              (list c-type def-name (syntax->list args)))
            syntax-e)
         (DS-domain)))

  ;; Define predicates to met in order to analyze the error type
  (define (met-cname=/dname=? ds-key)
    (match-define (list C-TYPE D-NAME _) ds-key)
    (and (bound-id=? LOOKED-DEF-C-TYPE C-TYPE)
         (bound-id=? LOOKED-DEF-NAME D-NAME)))

  (define (met-def-arity=? ds-key)
    (match-define (list _ _ ARGs-B-TYPE) ds-key)
    (eq? looked-def-arity (length ARGs-B-TYPE)))

  ;; Lets find entries in dom(DS) with same class and def name
  (define met-cname=/dname=-defs (filter met-cname=/dname=? def-dom))

  ;; I met zero criterion => unknown definition
  (when (empty? met-cname=/dname=-defs)
    (raise-unknown-def LOOKED-DEF-NAME LOOKED-DEF-C-TYPE))

  ;; Lets refine entries in dom(DS) with same arity
  (define met-cname=/dname=/arity=-defs
    (filter met-def-arity=? met-cname=/dname=-defs))

  ;; I met the cname=/dname=? criteria but not arity=? => arity error
  (when (empty? met-cname=/dname=/arity=-defs)
    (define expected-def (car met-cname=/dname=-defs))
    (match-define (list _ EXPECTED-DEF-NAME EXPECTED-DEF-ARGs) expected-def)
    (define expected-def-arity (length EXPECTED-DEF-ARGs))
    (raise-arity-error EXPECTED-DEF-NAME expected-def-arity LOOKED-DEF-ARGs))

  ;; Here, I met my three criterion => type mismatch
  (define expected-def (car met-cname=/dname=/arity=-defs))
  (match-define (list _ _ EXPECTED-DEF-ARGs) expected-def)
  (for ([looked-b-type LOOKED-DEF-ARGs]
        [expected-b-type EXPECTED-DEF-ARGs]
        [arg-pos (in-range (length LOOKED-DEF-ARGs))])
    (when (not (bound-id=? looked-b-type expected-b-type))
      ;; I found the type which is incorrect, and it is located a
      ;; position `arg-pos`.
      (define send-stx (current-syntax-context))
      (define ill-typed-arg (list-ref (syntax-e send-stx) (+ arg-pos 3)))
      (raise-type-mismatch looked-b-type expected-b-type
                           #:name ill-typed-arg)
      )))


;; Exceptions

;; Unknown type
(struct exn:unknown-type exn:fail:syntax ()
  #:extra-constructor-name make-exn:unknown-type
  #:transparent)

(define (raise-unknown-type B-TYPE)
  (define srcloc-msg (srcloc->string (build-source-location B-TYPE)))
  (define id (format "~s" (syntax->datum B-TYPE)))
  (define err-msg "unknown type in this scope")

  (raise (make-exn:unknown-type
          (string-append srcloc-msg ": " id ": " err-msg)
          (current-continuation-marks)
          (list (syntax-taint B-TYPE)))))

;; Type mismatch
(struct exn:type-mismatch exn:fail:syntax ()
  #:extra-constructor-name make-exn:type-mismatch
  #:transparent)

;; (: raise-type-mismatch  B-TYPE B-TYPE Syntax #:name (U Syntax Symbol #f) -> exn:type-mismatch)
(define (raise-type-mismatch GIVEN-B-TYPE EXPECTED-B-TYPE [context #f]
                             #:name [n #f])
  (define CTX (or context (current-syntax-context)))
  ;; (log-sclang-debug "Desugared syntax is ~.s" CTX)
  (define CTX-SURFACE (or (syntax-property CTX 'surface) CTX))
  (define srcloc-msg (srcloc->string (build-source-location CTX-SURFACE)))
  (define name (cond
                 [(syntax? n) (extract-exp-name (syntax-property n 'surface))]
                 [else n]))
  (define id (format "~s" (or name (extract-exp-name CTX-SURFACE))))
  (define err-msg "type mismatch")
  (define elab-msg
    (format (string-append "~n  The expression elaborate to the type ~s"
                           "~n  But the expected type is ~s, referring to declaration at ~a:~a"
                           "~n  in: ~.s")
            (syntax->datum GIVEN-B-TYPE)
            (syntax->datum EXPECTED-B-TYPE)
            (syntax-line EXPECTED-B-TYPE)
            (syntax-column EXPECTED-B-TYPE)
            (syntax->datum CTX-SURFACE)))

  (raise (make-exn:type-mismatch
          (string-append srcloc-msg ": " id ": " err-msg elab-msg)
          (current-continuation-marks)
          (list (syntax-taint CTX)))))

;; Def arity error
(struct exn:arity-error exn:fail:syntax ()
  #:extra-constructor-name make-exn:arity-error
  #:transparent)

;; (: raise-arity-error (Identifier Integer (Listof B-TYPE) STX -> exn:arity-error))
(define (raise-arity-error def expected-args-size given-args [context #f])
  (define CTX (or context (current-syntax-context)))
  ;; (log-sclang-debug "Desugared syntax is ~.s" CTX)
  (define CTX-SURFACE (or (syntax-property CTX 'surface) CTX))
  (define srcloc-msg (srcloc->string (build-source-location CTX-SURFACE)))
  (define id (format "~s" (extract-exp-name def)))
  (define err-msg "arity mismatch")
  (define given-args-size (length given-args))
  (define arity-msg
    (format (string-append "~n  def takes ~a but ~a supplied"
                           "~n  The def refers to declaration at ~a:~a"
                           "~n  in: ~.s")
            (format (if (<= expected-args-size 1) "~s argument" "~s arguments")
                    expected-args-size)
            (format (if (<= given-args-size 1) "~s was" "~s were")
                    given-args-size)
            (syntax-line def)
            (syntax-column def)
            (syntax->datum CTX-SURFACE)))

  (raise (make-exn:arity-error
          (string-append srcloc-msg ": " id ": " err-msg arity-msg)
          (current-continuation-marks)
          (list (syntax-taint CTX)))))

;; Unknown def
(struct exn:unknown-def exn:fail:syntax ()
  #:extra-constructor-name make-exn:unknown-def
  #:transparent)

;; (: raise-unknown-def (Identifier B-TYPE STX -> exn:unknown-def))
(define (raise-unknown-def def-name c-type [context #f])
  (define CTX (or context (current-syntax-context)))
  ;; (log-sclang-debug "Desugared syntax is ~.s" CTX)
  (define CTX-SURFACE (or (syntax-property CTX 'surface) CTX))
  (define srcloc-msg (srcloc->string (build-source-location CTX-SURFACE)))
  (define id (format "~s" (extract-exp-name def-name)))
  (define err-msg "unknown def")
  (define def-msg
    (format (string-append "~n  No def named ~s found for type ~s"
                           "~n  in: ~.s")
            (syntax->datum def-name)
            (syntax->datum c-type)
            (syntax->datum CTX-SURFACE)))

  (raise (make-exn:unknown-def
          (string-append srcloc-msg ": " id ": " err-msg def-msg)
          (current-continuation-marks)
          (list (syntax-taint CTX)))))

;; Unknown def
(struct exn:unknown-field exn:fail:syntax ()
  #:extra-constructor-name make-exn:unknown-field
  #:transparent)

;; (: raise-unknown-field (Identifier B-TYPE Syntax -> exn:unknown-field))
(define (raise-unknown-field field-name c-type [CONTEXT #f])
  (define CTX (or CONTEXT (current-syntax-context)))
  ;; (log-sclang-debug "Desugared syntax is ~.s" CTX)
  (define CTX-SURFACE (or (syntax-property CTX 'surface) CTX))
  (define srcloc-msg (srcloc->string (build-source-location CTX-SURFACE)))
  (define id (format "~s" (extract-exp-name field-name)))
  (define err-msg "unknown field")
  (define field-msg
    (format (string-append "~n  Class ~s has no field named ~s"
                           "~n  in: ~.s")
            (syntax->datum c-type)
            (syntax->datum field-name)
            (syntax->datum CTX-SURFACE)))

  (raise (make-exn:unknown-field
          (string-append srcloc-msg ": " id ": " err-msg field-msg)
          (current-continuation-marks)
          (list (syntax-taint CTX)))))


;; Utils

;; (Syntaxof a) -> (Syntaxof (Pairof (Syntaxof a) B-TYPE))
(define (get-τ stx)
  (with-syntax ([e-stx stx] [τ-stx (type-prop stx)])
    #'(e-stx τ-stx)))

;; (Syntaxof a) B-TYPE -> (Syntaxof a)
(define add-τ type-prop)

;; (: τ=? (B-TYPE B-TYPE -> Boolean))
(define τ=? bound-id=?)

(module+ test
  (define-test-suite utils
    (check-stx=? (get-τ (add-τ #'foo #'bar)) #'(foo bar))
    (check-stx=? (get-τ (add-τ (add-τ #'foo #'bar) #'baz)) #'(foo baz))
    (check-stx=? (get-τ #'foo) #'(foo #f))  ;; TODO: raise an exception?
    ))


;; Tests

(module+ test
  (require rackunit/text-ui
           (prefix-in env: (submod "env.rkt" basic-check test)))
  (provide simply-typed-tests)

  (define-check (check-τ stx b-type)
    (define stx-type (syntax-parse (get-τ stx) [(_ t) #'t]))

    (with-check-info*
      (list (make-check-name 'check-τ)
            (make-check-location (build-source-location-list stx))
            (make-check-actual stx-type)
            (make-check-expected b-type)
            #;(make-check-message
               (or msg
                   (format "#'~.a does not structurally equal to #'~.a"
                           (syntax->datum stx1)
                           (syntax->datum stx2)))))
      (thunk (check-true (τ=? stx-type b-type)))))

  (define simply-typed-tests
    (test-suite
     "Simple type checking phase"
     ;; Check env
     env:CS-tests
     env:Γ-tests
     env:FS-tests
     env:DS-tests
     ;; Check utils
     utils
     ;; Check phase rules
     ⊢τ-parse
     ⊢e-parse
     ⊢m-parse
     ))

  (run-tests simply-typed-tests)
  )


;; Bibliography
;;
;; @InProceedings{FKF98,
;;   author =       {Matthew Flatt and Shriram Krishnamurthi and Matthias
;;                   Felleisen},
;;   title =        {Classes and Mixins},
;;   booktitle =    {{POPL} '98, Proceedings of the 25th {ACM}
;;                   {SIGPLAN-SIGACT} Symposium on Principles of
;;                   Programming Languages, San Diego, CA, USA, January
;;                   19-21, 1998},
;;   year =         1998,
;;   pages =        {171--183},
;;   doi =          {10.1145/268946.268961},
;;   url =          {https://doi.org/10.1145/268946.268961},
;; }
