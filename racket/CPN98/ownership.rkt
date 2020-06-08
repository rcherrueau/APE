#lang racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.
;;
;; Ownership type checking phase (θ>)
;; - Type checks the program (for simple type -- "simple" as in simply
;;   typed λ calculus, i.e., no ownership).
;; - Based on [CPN98] (see Bibliography).
;;
;; Environments:
;;
;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects
;; - t^ is the ownership scheme of t
;;
;; Global:
;; - meta:CS is the set of defined ownership scheme
;; - meta:FS is the map of fields with ownership scheme field as value
;; - meta:DS is the map of definitions with return ownership sheme as
;;   value

(require (for-syntax racket/base)
         racket/function
         racket/match
         racket/sequence
         racket/syntax
         syntax/parse
         syntax/srcloc
         syntax/stx
         "definitions.rkt"
         "utils.rkt"
         "meta.rkt"
         (prefix-in env: (submod "env.rkt" ownership)))

(module+ test (require rackunit))

(provide θ>)


;; Phase θ>
(define-phase (θ> stx)
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Env

  ;; Set of local context parameters
  ;;
  ;; (: Σ (Setof Identifier))
  (Σ #:init '()
     #:mk env:make-Σ
     #:apply? [env:Σ-member? env:Σ-union]
     )

  ;; Mapping from locally bound variables to ownership types
  ;;
  ;; (: Γ (Identifier ~> O-TYPE))
  (Γ #:init   '()
     #:mk     env:make-Γ
     #:apply? [env:Γ-member? env:Γ-add env:Γ-ref]
     )

  ;; Store of the ownership scheme of the current class
  ;;
  ;; (: τ OW-SCHEME)
  (τ #:init #'Bottom
     #:mk   identity)

  ;; Mapping from existing class types to its ownership scheme
  ;; (t ~> t^)
  ;;
  ;; (: OWS (B-TYPE ~> OW-SCHEME))
  (OWS #:init   (meta-map-w/key
                (λ (kv)
                  (match-define (cons CTYPE CPARAM...) kv)
                  (cons CTYPE #`(#,CTYPE Θ #,CPARAM...)))
                meta:CS)
      #:mk     env:make-OWS
      #:apply? [env:OWS-arity env:OWS-ref env:ψ]
      )

  ;; Map of fields
  ;;
  ;; (: FS ((Syntaxof (Pairof B-TYPE        ; Class type
  ;;                          Identifier))  ; Field name
  ;;        ~> OW-SCHEME))                  ; Field return type
  (FS #:init meta:FS
      #:mk env:make-FS
      #:apply? [env:FS-member? env:FS-ref]
      )

  ;; Map of definitions
  ;;
  ;; (: DS ((Syntaxof (Pairof Identifier                     ; Class type
  ;;                          Identifier))                   ; Def name
  ;;        ~>
  ;;        (Syntaxof (Pairof (Syntaxof (Listof OW-SCHEME))  ; Type of def args
  ;;                          OW-SCHEME))                    ; Def return type
  (DS #:init (meta-map-w/key
              (λ (kv)
                (match-define (cons (list CTYPE NAME A-OWS...) R-OWS) kv)
                (cons #`(#,CTYPE . #,NAME) #`(#,A-OWS... #,R-OWS)))
              meta:DS)
      #:mk env:make-DS
      #:apply? [env:DS-member? env:DS-ref]
      )

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Parse
  (⊢p stx)
  )


;; ⊢p P : t
;;
;; `P` is well-formed with ownership type `t`
(define-rules ⊢p
  ;; [program]
  [(prog ~! CLASS ... E)
   ;; Check P ⊢d CLASS
   #:do [(stx-map ⊢d #'(CLASS ...))]
   ;; Check P,[],[] ⊢e E : t
   #:with t (get-τ (with-Σ '() (with-Γ '() (⊢e #'E))))
   ;; ----------------------------------------------------------------
   ;; ⊢p P : t
   ;; TODO:
   ;; (add-τ this-syntax #'t)
   this-syntax
   ])


;; P ⊢d CLASS
;;
;; In context `P`, `CLASS` is well-formed
(define-rules ⊢d
  ;; [Class]
  [(class ~! NAME [CPARAM ...] FIELD/DEF ...)
   #:with [FIELD ...] (filter field? (stx->list #'(FIELD/DEF ...)))
   #:with [DEF ...] (filter def? (stx->list #'(FIELD/DEF ...)))
   ;; Σ = { Θ } ∪ { CPARAM ... }
   ;; τ = OWS(NAME) (i.e., `#'(NAME Θ [CPARAM ...])`)
   ;;;; The `Θ` is a variable that is going to be substitute later.
   #:do [(define the-Σ #'(Θ CPARAM ...))
         (define the-τ (OWS-ref #'NAME))]
   ;; Check P,Σ ⊢τ t on fields
   #:with [(field ~! F-NAME F) ...] #'(FIELD ...)
   #:when (with-Σ the-Σ
            (stx-for/and ([t #'(F ...)]) (⊢τ t)))
   ;; Check P,Σ,{ this: NAME<Θ|[CPARAM ...]> } ⊢m DEF
   ;;;; Note: Unlike [CPN98], I chose to go with an environment for
   ;;;; the current class ownership scheme (τ), rather than binding
   ;;;; the `this` in `Γ` here, so I don't have to latter implement an
   ;;;; union operation for `Γ`.
   #:when (with-Σ the-Σ
            (with-τ the-τ
              (stx-map ⊢m #'(DEF ...))))
   ;; ----------------------------------------------------------------
   ;; P ⊢d (class NAME [CPARAM ...] FIELD ... DEF ...)
   this-syntax])


;; P,Σ,τ ⊢m DEF
;;
;; In context `P,Σ,τ`, `DEF` is well-formed
(define-rules ⊢m
  ;; [Method]
  [(def ~! (_NAME (ARG-NAME ARG-SCHEME) ... RET-SCHEME) E ...+)
   ;; Get current class type store in τ environment
   #:with τ0 (τ)
   ;; Check P,Σ ⊢τ t on return type
   #:when (⊢τ #'RET-SCHEME)
   ;; Check P,Σ ⊢τ t on args
   #:when (stx-for/and ([t #'(ARG-SCHEME ...)]) (⊢τ t))
   ;; Check P,Σ,{this: τ0, ARG-NAME: ARG-SCHEME, ...} ⊢e E ... LEB : RET-SCHEME
   #:with [_ ... ΘLEB] (with-Γ #'{ (this     . τ0)
                                   (???      . RET-SCHEME)
                                   (ARG-NAME . ARG-SCHEME) ... }
                             (stx-map ⊢e #'(E ...)))
   #:with [_ t-e] (get-τ #'ΘLEB)
   #:when (or (τ=? #'t-e #'RET-SCHEME)
              (raise (mk-exn:ownership-mismatch #'t-e #'RET-SCHEME #'ΘLEB)))
   ;; ----------------------------------------------------------------
   ;; P,Σ,τ0 ⊢m (def (NAME (ARG-NAME ARG-SCHEME) ... RET-SCHEME) E ...+)
   this-syntax])

(module+ test
  (define-test-suite ⊢m-parse
    ;; P,Σ,τ ⊢m DEF
    (with-OWS (list (cons #'Foo #'(Foo Θ ())) (cons #'Bar #'(Bar Θ (n m))))
    (with-Σ #'(o n m)
    (with-τ #'(Foo o ())

      ;; Check P,Σ ⊢τ t on return type
      (check-exn exn:unknown-cparam?
                 (thunk (⊢m #'(def (def/0 (Bar o (n t))) _)))
                 "`t` is not a defined context parameter")
      (check-exn exn:arity-error?
                 (thunk (⊢m #'(def (def/2 (Bar o (n))) _)))
                 "`Bar` takes two context parameters")
      (check-exn exn:arity-error?
                 (thunk (⊢m #'(def (def/2 (Bar o (n n n))) _)))
                 "`Bar` takes two context parameters")

      ;; Check P,Σ ⊢τ t on args
      (check-exn exn:unknown-cparam?
                 (thunk (⊢m #'(def (def/2 (arg1 (Foo o ())) (arg2 (Bar o (n t))) (Bar o (n m))) _)))
                 "`t` is not a defined context parameter")
      (check-exn exn:unknown-cparam?
                 (thunk (⊢m #'(def (def/2 (arg1 (Foo t ())) (arg2 (Bar o (n m))) (Bar o (n m))) _)))
                 "`t` is not a defined context parameter")
      (check-exn exn:arity-error?
                 (thunk (⊢m #'(def (def/2 (arg1 (Foo o ())) (arg2 (Bar o (n))) (Bar o (n m))) _)))
                 "`Bar` takes two context parameters")
      (check-exn exn:arity-error?
                 (thunk (⊢m #'(def (def/2 (arg1 (Foo o ())) (arg2 (Bar o (n n n))) (Bar o (n m))) _)))
                 "`Bar` takes two context parameters")

      ;; Check P,Σ,{this: τ0, ARG-NAME: ARG-SCHEME, ...} ⊢e E ... LEB : RET-SCHEME
      (check-exn exn:ownership-mismatch?
                 (thunk (⊢m #'(def (def/0 (Bar o (n m)))
                                (new (Bar n (n m))))))
                 "`def` should return a `o/Bar{n m}` but the expression is of type `n/Bar{n m}`")
      (check-exn exn:ownership-mismatch?
                 (thunk (⊢m #'(def (def/0 (Bar o (n m)))
                                (new (Bar o (m m))))))
                 "`def` should return a `o/Bar{n m}` but the expression is of type `n/Bar{m m}`")
      (check-not-exn
       (thunk (⊢m #'(def (def/0 (Foo o ())) this)))
       "`this` is bound in the expression with the `Foo` type (τ env)")
      (check-not-exn
       (thunk (⊢m #'(def (def/0 (Foo o ())) ???)))
       "A def accepts the `???` place holder as expression" )
      (check-not-exn
       (thunk (⊢m #'(def (def/0 (Bar o (n m))) ???)))
       "A def accepts the `???` place holder as expression" )
      (check-not-exn
       (thunk (⊢m #'(def (def/2 (arg1 (Bar o (n m))) (arg2 (Foo o ())) (Bar o (n m))) arg1)))
       "`arg1` is bound in the expression with the `o/Bar{n m}` type")
      (check-not-exn
       (thunk (⊢m #'(def (def/2 (arg1 (Bar o (n m))) (arg2 (Foo o ())) (Foo o ())) arg2)))
       "`arg2` is bound in the expression with the `o/Foo` type")
      (check-not-exn
       (thunk (⊢m #'(def (def/2 (arg1 (Bar o (n m))) (arg2 (Foo o ())) (Foo o ())) arg1 arg2)))
       "The type of the BODY is the type of the LEB")
      (check-not-exn
       (thunk (⊢m #'(def (def/2 (arg1 (Bar o (n m))) (arg2 (Foo o ())) (Bar o (n m))) arg2 arg1)))
       "The type of the BODY is the type of the LEB")
      )))))


;; P,Σ,Γ ⊢e E : t
;;
;; In context `P,Γ`, `E` elaborates to `?E` and has type `t`
(define-rules ⊢e
  ;; [New]
  [(new ~! SCHEME:ow-scheme)
   ;; Check P,Σ ⊢τ SCHEME-TYPE
   #:when (⊢τ #'SCHEME)
   ;; ----------------------------------------------------------------
   ;; P,Σ,Γ ⊢e (new t) : t
   (add-τ this-syntax #'SCHEME)]

  ;; [Local Access]
  [ID:id
   ;; Check ID ∈ dom(Γ)
   #:when (or (Γ-member? #'ID)
              ;; Unbound identifier. This is definitely not supposed
              ;; to happened thanks to both desugaring and simple type
              ;; check, but who knows ...
              (raise-syntax-error #f "unbound identifier" #'ID))
   ;; ----------------------------------------------------------------
   ;; P,Σ,Γ ⊢e ID : Γ(ID)
   (add-τ this-syntax (Γ-ref #'ID))]

  ;; [Field Access]
  ;;
  ;; Type checking example:
  ;;
  ;; (class Bar)
  ;; (class Foo{n} (field [bar : n/Bar]))
  ;; (get-field (new Foo{rep}) bar)
  ;;  ~~~~~~~~~  ~~~~~~~~~~~~  ~~~
  ;;  │          │             ╰ n/Bar  from FS(t-e.TYPE . foo)
  ;;  │          │                      and t-e.TYPE is Foo
  ;;  │          │
  ;;  │          ╰ world/Foo{rep}   from P,Σ,Γ ⊢e E : t-e
  ;;  │
  ;;  ╰ rep/Bar  from the substitution of `n`
  ;;             by `rep`. The substitution table
  ;;             is Ψ(t-e) = { Θ -> world, n -> rep }
  ;;             built using the t-e ownership scheme
  ;;             Θ/Foo{n} and ownership type world/Foo{rep}.
  ;;
  ;; Moreover we have to check the /static visibility/ of the `bar`
  ;; field in the context of the `(new Foo{rep})` expression. In other
  ;; words, is it OK for the expression `(new Foo{rep})` to access the
  ;; `bar` field.
  [(get-field ~! E FNAME)
   ;; Check P,Σ,Γ ⊢e E : t-e
   #:with [_ t-e:ow-scheme] (get-τ (⊢e #'E))
   ;; σ = ψ(t-e)
   #:do [(define σ (curry env:σ (ψ #'t-e)))]
   ;; t-field = FS(t-e.TYPE . foo)  ; Get the type of the field
   #:with t-field (FS-ref #'(t-e.TYPE . FNAME))
   ;; Check SV(E, t-field)
   ;;;; Is expression `E` allowed to access field `FNAME`?
   #:when (or (visible? #'E #'t-field)
              (raise (mk-exn:visibility-error #'E #'t-field)))
   ;; ----------------------------------------------------------------
   ;; P,Σ,Γ ⊢e (get-fieldield E FNAME) : σ(t-field)
   (add-τ this-syntax (σ #'t-field))]

  ;; [Field Update]
  ;;
  ;; Type checking example:
  ;;
  ;; (class Bar)
  ;; (class Foo{n} (field [bar : n/Bar]))
  ;; (set-field (new Foo{rep}) bar (new rep/Bar))
  ;;  ~~~~~~~~~  ~~~~~~~~~~~~  ~~~  ~~~~~~~~~~~
  ;;  │          │             │    ╰ rep/Bar                         (*)
  ;;  │          │             ╰ n/Bar  from FS(t-e.TYPE . foo)
  ;;  │          │                      and t-e.TYPE is Foo
  ;;  │          │
  ;;  │          ╰ world/Foo{rep}   from P,Σ,Γ ⊢e E : t-e
  ;;  │
  ;;  ╰ rep/Bar  from the substitution of `n`                         (†)
  ;;             by `rep`. The substitution table
  ;;             is Ψ(t-e) = { Θ -> world, n -> rep }
  ;;             built using the t-e ownership scheme
  ;;             Θ/Foo{n} and ownership type world/Foo{rep}.
  ;;
  ;; Ensure that type (†) is equivalent to type (*). Intuitively, it
  ;; ensures that the BODY of the set-field fits into the field.
  ;;
  ;; Moreover we have to check the /static visibility/ of the `bar`
  ;; field in the context of the `(new Foo{rep})` expression. In other
  ;; words, is it OK for the expression `(new Foo{rep})` to access the
  ;; `bar` field.
  ;;
  [(set-field! ~! E FNAME BODY)
   ;; Check P,Σ,Γ ⊢e E t-e
   #:with [_ t-e:ow-scheme] (get-τ (⊢e #'E))
   ;; σ = ψ(t-e)
   #:do [(define σ (curry env:σ (ψ #'t-e)))]
   ;; t-field = FS(t-e.TYPE . foo)  ; Get the type of the field
   #:with t-field (FS-ref #'(t-e.TYPE . FNAME))
   ;; Check P,Σ,Γ ⊢e BODY : σ(t-field)
   ;;;; The body as to elaborate into something that fit into the
   ;;;; field
   #:with [_ t-body] (get-τ (⊢e #'BODY))
   #:when (or (τ=? #'t-body (σ #'t-field))
              (raise (mk-exn:ownership-mismatch #'t-body (σ #'t-field) #'E)))
   ;; Check SV(E, t-field)
   ;;;; Is object of field `FNAME` visible to `E`?
   #:when (or (visible? #'E #'t-field)
              (raise (mk-exn:visibility-error #'E #'t-field)))
   ;; ----------------------------------------------------------------
   ;; P,Σ,Γ ⊢e (set-field (E : t-e) FNAME BODY) : σ(t-field)
   (add-τ this-syntax (σ #'t-field))]

  ;; [Method Call]
  [(send ~! E DNAME PARAM ...)
   ;; Check P,Σ,Γ ⊢e E t-e
   #:with [_ t-e:ow-scheme] (get-τ (⊢e #'E))
   ;; σ = ψ(t-e)
   #:do [(define σ (curry env:σ (ψ #'t-e)))]
   ;; t-arg ... → t-ret = DS(t-e.TYPE . foo)  ; Get args and the return type of the def
   #:with [(t-arg ...) t-ret] (DS-ref #'(t-e.TYPE . DNAME))
   ;; Check P,Σ,Γ ⊢e (PARAM : σ(t-arg)) ...
   ;;;; Expressions pass at `DNAME` call should fit into `DNAME`
   ;;;; arguments
   #:with [(_ t-param) ...] (stx-map (∘ get-τ ⊢e) #'(PARAM ...))
   #:when (stx-for/and ([t-param #'(t-param ...)]
                        [t-arg   #'(t-arg ...)])
            (or (τ=? t-param (σ t-arg))
                (raise (mk-exn:ownership-mismatch t-param (σ t-arg)))))
   ;; Check SV(E, t-param) ...
   ;;;; Are arguments pass to `DNAME` visible to `E`?
   ;;;;
   ;;;; TODO: I don't really understand the relevance of that one.  I
   ;;;; should find an example.
   #:when (stx-for/and ([t-arg #'(t-arg ...)])
            (or (visible? #'E #'t-arg)
                (raise (mk-exn:visibility-error #'E #'t-arg))))
   ;; Check SV(E, t-field)
   ;;;; Is object returned by `DNAME` visible to `E`?
   #:when (or (visible? #'E #'t-ret)
              (raise (mk-exn:visibility-error #'E #'t-ret)))
   ;; ----------------------------------------------------------------
   ;; Check P,Σ,Γ ⊢e (send (E : t) DNAME PARAM ...) : σ(t-ret)
   (add-τ this-syntax (σ #'t-ret))]

  ;; [Local Update, Sequence]
  ;;
  ;; Note: The [CPN98] does not rely on `let` binding for local
  ;; update, but instead something more imperative: `VAR-NAME = E`.
  ;; It also does not require the `VAR-NAME` to have a type, but
  ;; rather infers it from the expression (`E`).  This is a bit
  ;; different here.  Actually, I must say that I don't understand the
  ;; [Local Update] rule of [CPN98].  It requires `VAR-NAME` to be
  ;; bound as a premise, i.e., `VAR-NAME ∈ dom(Γ)`.  That sounds weird
  ;; because no rules introduce `VAR-NAME` into `Γ`.  The definition
  ;; of [Local Update] of [CPN98] sounds rather denotational than
  ;; operational.  I would expect to find a kind of good definition in
  ;; [IPW01] for local update, but the expression surprisingly does
  ;; not contains a variable assignment.  As a side note, this weird
  ;; definition really shows that the `let` notation is a must for
  ;; variable binding because it makes it clear that `VAR-NAME` is
  ;; bound in the `BODY`.
  [(let ~! (VAR-NAME VAR-SCHEME E) BODY ...)
   ;; Check P,Σ ⊢τ VAR-SCHEME
   #:when (⊢τ #'VAR-SCHEME)
   ;; Check  P,Σ,Γ ⊢e E : VAR-SCHEME
   #:with [_ t] (get-τ (with-Γ (Γ-add #'(??? . VAR-SCHEME))
                         (⊢e #'E)))
   #:when (or (τ=? #'t #'VAR-SCHEME)
              (raise (mk-exn:ownership-mismatch #'t #'VAR-SCHEME #'E)))
   ;; Check P,Σ,{VAR-NAME: VAR-SCHEME, ...} ⊢e E ... LEB : t-leb
   #:with [_ ... LEB] (with-Γ #'{ (VAR-NAME . VAR-SCHEME) }
                         (stx-map ⊢e #'(BODY ...)))
   #:with [_ t-leb] (get-τ #'LEB)
   ;; ------------------------------------------------------------------
   ;; P,Γ ⊢e let (VAR-NAME VAR-OW-SCHEME E) BODY ... LEB : t-leb
   (add-τ this-syntax #'t-leb)])

(module+ test
  (define-test-suite ⊢e-parse
    ;; P,Σ,Γ ⊢e E : t
    (with-OWS (list (cons #'Foo #'(Foo Θ ())) (cons #'Bar #'(Bar Θ (ν μ))))
    (with-FS  (list (cons #'(Foo . bar) #'Bar))
    (with-DS  (list (cons #'(Foo . def/0) #'(() (Bar o (n m))))
                    (cons #'(Foo . def/2) #'(((Foo o ()) (Bar o (n m))) (Bar o (n m)))))
    (with-Σ #'(n m)
    (with-Γ #'{ (this . (Foo Θ ())) (_ . (Bar o (n m))) }

      ;; ;; [New]             (new ~! SCHEME:ow-scheme)
      ;; ;; Check P,Σ ⊢τ SCHEME-TYPE
      ;; (check-exn exn:unknown-type? (thunk (⊢e #'(new (Baz o ())))))
      ;; (check-not-exn (thunk (⊢e #'(new (Foo o ())))))
      ;; ;; P,Σ,Γ ⊢e (new t) : t
      ;; (check-τ (⊢e #'(new (Foo o ()))) #'Foo)
      ;; (check-τ (⊢e #'(new (Foo p ()))) #'Foo)
      ;; (check-τ (⊢e #'(new (Foo p (c)))) #'Foo)

      ;; ;; [Local Access]    ID
      ;; ;; Check ID ∈ dom(Γ)
      ;; (check-not-exn (thunk (⊢e #'this)))
      ;; (check-not-exn (thunk (⊢e #'_)))
      ;; (check-exn exn:fail:syntax? (thunk (⊢e #'baz)))
      ;; ;; P,Σ,Γ ⊢e ID : Γ(ID)
      ;; (check-τ (⊢e #'this) #'Foo)
      ;; (check-τ (⊢e #'_)  #'Bar)

      ;; ;; [Field Access]    (get-field ~! E FNAME)
      ;; ;; Check P,Σ,Γ ⊢e E : t-e
      ;; (check-exn exn:unknown-field? (thunk (⊢e #'(get-field (new (Bar o ())) bar))))
      ;; (check-exn exn:unknown-field? (thunk (⊢e #'(get-field (new (Foo o ())) g))))
      ;; (check-exn exn:unknown-field? (thunk (⊢e #'(get-field _ bar))))
      ;; (check-not-exn (thunk (⊢e #'(get-field this bar))))
      ;; (check-not-exn (thunk (⊢e #'(get-field (new (Foo o ())) bar))))
      ;; (check-not-exn (thunk (⊢e #'(get-field (new (Foo p ())) bar))))   ;; <| Only look at
      ;; (check-not-exn (thunk (⊢e #'(get-field (new (Foo p (c))) bar))))  ;;  | basic type.
      ;; ;; Check SV(E, t-field)
      ;; ;; P,Σ,Γ ⊢e (get-fieldield E FNAME) : σ(t-field)
      ;; (check-τ (⊢e #'(get-field (new (Foo o ())) bar)) #'Bar)
      ;; (check-τ (⊢e #'(get-field this bar)) #'Bar)

      ;; ;; [Field Update]    (set-field! ~! E FNAME BODY)
      ;; ;; Check P,Σ,Γ ⊢e E t-e
      ;; ;; Check P,Σ,Γ ⊢e BODY : σ(t-field)
      ;; (check-exn exn:unknown-field?
      ;;            (thunk (⊢e #'(set-field! (new (Foo o ())) g _))))
      ;; (check-exn exn:unknown-field?
      ;;            (thunk (⊢e #'(set-field! (new (Bar o ())) bar _))))
      ;; (check-exn exn:unknown-field?
      ;;            (thunk (⊢e #'(set-field! _ bar _))))
      ;; ;; Check SV(E, t-field)
      ;; ;; P,Σ,Γ ⊢e (set-field (E : t-e) FNAME BODY) : σ(t-field)
      ;; (check-exn exn:type-mismatch?
      ;;            (thunk (⊢e #'(set-field! (new (Foo o ())) bar (new (Foo o ()))))))
      ;; (check-exn exn:type-mismatch?
      ;;            (thunk (⊢e #'(set-field! (new (Foo o ())) bar this))))
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(set-field! (new (Foo o ())) bar _))))
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(set-field! (new (Foo o ())) bar (new (Bar o ()))))))
      ;; (check-not-exn                                                       ;; <| Only look at
      ;;  (thunk (⊢e #'(set-field! (new (Foo o ())) bar (new (Bar p ()))))))  ;;  | basic type,
      ;; (check-not-exn                                                       ;;  | not owner
      ;;  (thunk (⊢e #'(set-field! (new (Foo o ())) bar (new (Bar p (c))))))) ;;  | nor ctx params
      ;; (check-τ (⊢e #'(set-field! (new (Foo o ())) bar (new (Bar o ())))) #'Bar)
      ;; (check-τ (⊢e #'(set-field! (new (Foo o ())) bar _)) #'Bar)

      ;; [Method Call]     (send ~! E DNAME PARAM ...)
      ;; Check P,Σ,Γ ⊢e E t-e
      ;; Check P,Σ,Γ ⊢e (PARAM : σ(t-arg)) ...
      ;;;; Expressions pass at `DNAME` call should fit into `DNAME`
      ;;;; arguments
      ;; Check SV(E, t-param) ...
      ;;;; Are arguments pass to `DNAME` visible to `E`?
      ;; Check SV(E, t-field)
      ;;;; Is object returned by `DNAME` visible to `E`?
      ;; Check P,Σ,Γ ⊢e (send (E : t) DNAME PARAM ...) : σ(t-ret)
      ;;;; P,Γ ⊢e (send E DNAME PARAM ...) ≫
      ;;;;          (send (?E : t) DNAME ?PARAM ...) : DS(t DNAME PARAM ...)

      ;; (check-exn exn:unknown-def?
      ;;            (thunk (⊢e #'(send (new (Foo o ())) udef))))
      ;; (check-exn exn:arity-error?
      ;;            (thunk (⊢e #'(send (new (Foo o ())) def/0 _))))
      ;; (check-exn exn:arity-error?
      ;;            (thunk (⊢e #'(send (new (Foo o ())) def/0 _ _))))
      ;; (check-exn exn:arity-error?
      ;;            (thunk (⊢e #'(send (new (Foo o ())) def/2))))
      ;; (check-exn exn:arity-error?
      ;;            (thunk (⊢e #'(send (new (Foo o ())) def/2 _))))
      ;; (check-exn exn:arity-error?
      ;;            (thunk (⊢e #'(send (new (Foo o ())) def/2 _ _ _))))
      ;; (check-exn exn:type-mismatch?
      ;;            (thunk (⊢e #'(send (new (Foo o ())) def/2 _ _))))
      ;; (check-exn exn:type-mismatch?
      ;;            (thunk (⊢e #'(send (new (Foo o ())) def/2 this this))))
      ;; (check-exn exn:type-mismatch?
      ;;            (thunk (⊢e #'(send (new (Foo o ())) def/2 _ this))))
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(send (new (Foo o ())) def/0))))
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(send (new (Foo o ())) def/2 this _))))
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(send (new (Foo o ())) def/2 (new (Foo o ())) (new (Bar o ()))))))
      ;; ;;;; Only look at basic type, not owner nor ctx params.
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(send (new (Foo o ())) def/2 (new (Foo p ())) (new (Bar p ()))))))
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(send (new (Foo o ())) def/2 (new (Foo p (c))) (new (Bar p (c)))))))
      ;; (check-τ (⊢e #'(send (new (Foo o ())) def/0)) #'Bar)
      ;; (check-τ
      ;;  (⊢e #'(send (new (Foo o ())) def/2 (new (Foo o ())) (new (Bar o ()))))
      ;;  #'Bar)
      ;; (check-τ (⊢e #'(send (new (Foo o ())) def/2 this _)) #'Bar)

      ;; [Local Update, Sequence]  (let ~! (VAR-NAME VAR-SCHEME E) BODY ...)
      ;; Check P,Σ ⊢τ VAR-SCHEME
      ;; Check  P,Σ,Γ ⊢e E : VAR-SCHEME  (τ=?)
      ;; Check P,Σ,{VAR-NAME: VAR-SCHEME, ...} ⊢e E ... LEB : t-leb
      ;; P,Γ ⊢e let (VAR-NAME VAR-OW-SCHEME E) BODY ... LEB : t-leb

      ;; ;;;; Check P ⊢τ VAR-SCHEME-TYPE
      ;; (check-exn exn:unknown-type? (thunk (⊢e #'(let (baz (Baz o ()) _) _))))
      ;; ;;;; Check  P,Γ ⊢e E ≫ ?E : VAR-OW-SCHEME
      ;; (check-exn exn:type-mismatch? (thunk (⊢e #'(let (baz (Foo o ()) (new (Bar o ()))) _))))
      ;; ;;;; Check P,Γ{VAR-NAME: VAR-OW-SCHEME} ⊢e BODY ... LEB ≫ ?BODY ... ?LEB : t-body
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(let (foo (Foo o ()) this) foo))))
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(let (foo (Foo o ()) this) _))))
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(let (foo (Foo o ()) this) _ _))))
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(let (foo (Foo o ()) ???) _)))
      ;;  "A let binding accepts the `???` as expression")
      ;; (check-not-exn
      ;;  (thunk (⊢e #'(let (foo (Foo o ())  (new (Foo o ()))) _))))
      ;; (check-not-exn                                               ;; <| Only look at
      ;;  (thunk (⊢e #'(let (foo (Foo p ())  (new (Foo o ()))) _))))  ;;  | basic type,
      ;; (check-not-exn                                               ;;  | not owner
      ;;  (thunk (⊢e #'(let (foo (Foo p (c)) (new (Foo o ()))) _))))  ;;  | nor ctx params
      ;; ;;;; P,Γ ⊢e *let (VAR-NAME VAR-OW-SCHEME E) BODY ... LEB ≫
      ;; ;;;;           *let (VAR-NAME VAR-OW-SCHEME ?E) ?BODY ... ?LEB : t-body
      ;; (check-τ (⊢e #'(let (foo (Foo o ()) ???) foo))   #'Foo)
      ;; (check-τ (⊢e #'(let (foo (Foo o ()) ???) _))     #'Bar)
      ;; (check-τ (⊢e #'(let (foo (Foo o ()) ???) foo _)) #'Bar)
      ;; (check-τ (⊢e #'(let (foo (Foo o ()) ???) _ foo)) #'Foo)
      ;; (check-τ (⊢e #'(let (foo (Foo o ()) ???) _ _))   #'Bar)
      ;; (check-τ (⊢e #'(let (foo (Foo o ()) ???) (let (bar (Bar o ()) ???) bar))) #'Bar)
      ;; (check-τ (⊢e #'(let (foo (Foo o ()) ???) (let (foo (Bar o ()) ???) foo)))
      ;;          #'Bar
      ;;          "A inner `let` shadows bindings of the outer `let`s")
      )))))))


;; P,Σ ⊢τ t
;;
;; In context `P` and with local context parameters `Σ`, `t` is well
;; formed.
;;
;; [Type]
(define-rules ⊢τ
  [t:ow-scheme
   #:with [CPARAM ...] #'t.CPARAMS
   ;; Check |{CPARAM ...}| = OWS(TYPE)
   ;;;; We provide enough context parameters for that class.  For
   ;;;; whatever reason, this isn't checked in [CPN98].
   #:when (let ([class-cparams-size (OWS-arity #'t.TYPE)]
                [type-cparams-size (length (syntax->list #'t.CPARAMS))])
            (or (eq? class-cparams-size type-cparams-size)
                (raise (mk-exn:arity-error class-cparams-size type-cparams-size))))
   ;; Check {OWNER CPARAM ...} ∈ Σ ∪ {rep world}
   #:when (with-Σ (Σ-union #'(rep world))
            (stx-for/and ([cparam #'(t.OWNER CPARAM ...)])
              (or (Σ-member? cparam)
                  (raise (mk-exn:unknown-cparam cparam)))))
   ;; ------------------------------------------------------------------
   ;; P,Σ ⊢τ (TYPE OWNER {CPARAM ...})
   this-syntax])

(module+ test
  (define-test-suite ⊢τ-parse
    (with-OWS (list (cons #'Foo #'(Foo Θ ())) (cons #'Bar #'(Bar Θ (n m))))
    (with-Σ #'(n m)
      (check-not-exn (thunk (⊢τ #'(Foo rep   ()))))
      (check-not-exn (thunk (⊢τ #'(Foo world ()))))
      (check-not-exn (thunk (⊢τ #'(Foo n     ()))))
      (check-not-exn (thunk (⊢τ #'(Foo m     ()))))
      (check-not-exn (thunk (⊢τ #'(Bar rep   (n n)))))
      (check-not-exn (thunk (⊢τ #'(Bar rep   (n m)))))
      (check-not-exn (thunk (⊢τ #'(Bar rep   (m n)))))
      (check-not-exn (thunk (⊢τ #'(Bar rep   (rep rep)))))
      (check-not-exn (thunk (⊢τ #'(Bar rep   (world world)))))
      (check-exn exn:arity-error? (thunk (⊢τ #'(Foo world (n)))))
      (check-exn exn:arity-error? (thunk (⊢τ #'(Bar world (n n n)))))
      (check-exn exn:unknown-cparam? (thunk (⊢τ #'(Foo o ()))))
      (check-exn exn:unknown-cparam? (thunk (⊢τ #'(Bar rep (rep o)))))
      ))))


;; Exceptions

;; Wrong number of context parameters
(struct exn:arity-error exn:fail:syntax ()
  #:transparent)

;; (: mk-exn:arity-error ((Identifier Integer) ([U Syntax #f]) . ->* . exn:arity-error))
(define (mk-exn:arity-error expected-cparam-size given-cparam-size [context #f])
  (define CTX (or context (current-syntax-context)))
  (log-sclang-debug "Desugared syntax is ~.s" CTX)
  (define CTX-SURFACE (or (syntax-property CTX 'surface) CTX))
  (log-sclang-debug "Surface syntax is ~.s" CTX-SURFACE)
  (define srcloc-msg (srcloc->string (build-source-location CTX-SURFACE)))
  (define id (format "~s" (extract-exp-name CTX)))
  (define err-msg "wrong number of context parameters")
  (define arity-msg
    (format (string-append "~n  expected ~a, found ~a"
                           "~n  in: ~.s")
            expected-cparam-size given-cparam-size
            (syntax->datum CTX-SURFACE)))

  (exn:arity-error
   (string-append srcloc-msg ": " id ": " err-msg arity-msg)
   (current-continuation-marks)
   (list (syntax-taint CTX))))

;; Unknown context parameter
(struct exn:unknown-cparam exn:fail:syntax ()
  #:transparent)

;; (: mk-exn:unknown-cparam ((Identifer) ([U Syntax #f]) . ->* . exn:unknown-cparam))
(define (mk-exn:unknown-cparam CPARAM [context #f])
  (define CTX (or context (current-syntax-context)))
  (log-sclang-debug "Desugared syntax is ~.s" CTX)
  (define CTX-SURFACE (or (syntax-property CTX 'surface) CTX))
  (log-sclang-debug "Surface syntax is ~.s" CTX-SURFACE)
  (define srcloc-msg (srcloc->string (build-source-location CPARAM)))
  (define id (format "~s" (syntax->datum CPARAM)))
  (define err-msg
    (format (string-append "unknown context parameter in this scope"
                           "~n  in: ~.s")
            (syntax->datum CTX-SURFACE)))

  (exn:unknown-cparam
   (string-append srcloc-msg ": " id ": " err-msg)
   (current-continuation-marks)
   (list (syntax-taint CPARAM))))

;; Ownership mismatch at type checking
(struct exn:ownership-mismatch exn:fail:syntax ()
  #:transparent)

;; (: mk-exn:ownership-mismatch
;;   ((OW-SCHEME OW-SCHEME)
;;    ((U Syntax #f))
;;    . ->* . exn:owner-mismatch))
(define (mk-exn:ownership-mismatch GIVEN-OW-SCHEME EXPECTED-OW-SCHEME [context #f])
  (define CTX (or context (current-syntax-context)))
  ;; (log-sclang-debug "Desugared syntax is ~.s" CTX)
  (define CTX-SURFACE (or (syntax-property CTX 'surface) CTX))
  ;; (log-sclang-debug "Surface syntax is ~.s" CTX-SURFACE)
  (define srcloc-msg (srcloc->string (build-source-location CTX-SURFACE)))
  (define id (format "~s" (extract-exp-name CTX-SURFACE)))
  (define err-msg "owner mismatch")
  (define elab-msg
    (format (string-append "~n  The expression elaborate to the ownership ~s"
                           "~n  But the expected ownership is ~s, referring to declaration at ~a:~a"
                           "~n  in: ~.s")
            (syntax->datum GIVEN-OW-SCHEME)
            (syntax->datum EXPECTED-OW-SCHEME)
            (syntax-line EXPECTED-OW-SCHEME)
            (syntax-column EXPECTED-OW-SCHEME)
            (syntax->datum CTX-SURFACE)))

  (exn:ownership-mismatch
   (string-append srcloc-msg ": " id ": " err-msg elab-msg)
   (current-continuation-marks)
   (list (syntax-taint CTX))))

;; Visibility error of an expression
(struct exn:visibility-error exn:fail:syntax ()
  #:transparent)

;; (: mk-exn:visibility-error
;;   ((Syntax OW-SCHEME)
;;    ((U Syntax #f))
;;    . ->* . exn:visibility-error))
(define (mk-exn:visibility-error E OW-TYPE [context #f])
  (define CTX (or context (current-syntax-context)))
  ;; (log-sclang-debug "Desugared syntax is ~.s" CTX)
  (define CTX-SURFACE (or (syntax-property CTX 'surface) CTX))
  ;; (log-sclang-debug "Surface syntax is ~.s" CTX-SURFACE)
  (define srcloc-msg (srcloc->string (build-source-location CTX-SURFACE)))
  (define id (format "~s" (extract-exp-name CTX-SURFACE)))
  (define err-msg "visibility error")
  (define visibility-msg
    (format (string-append "~n  The expression is not allowed to access object of type ~s"
                           "~n  at: ~.s"
                           "~n  in: ~.s")
            (syntax->datum (or (syntax-property OW-TYPE 'surface) OW-TYPE))
            (syntax->datum (or (syntax-property E 'surface) E))
            (syntax->datum CTX-SURFACE)))

  (exn:visibility-error
   (string-append srcloc-msg ": " id ": " err-msg visibility-msg)
   (current-continuation-marks)
   (list (syntax-taint CTX))))


;; Utils

;; (Syntaxof a) -> (Syntaxof (Pairof (Syntaxof a) OW-SCHEME))
;;
;; Note: I should raise a syntax error if the ow-scheme-prop is
;; false. For sure, having a term with no type when it should have one
;; is an error that should stop the computation.
(define (get-τ stx)
  (with-syntax ([the-stx stx]
                [τ-stx (ow-scheme-prop stx)])
    #'(the-stx τ-stx)))

;; (Syntaxof a) OW-SCHEME -> (Syntaxof a)
(define add-τ ow-scheme-prop)

;; (: τ=? (OW-SCHEME OW-SCHEME -> Boolean))
(define (τ=? stx1 stx2)
  (define/syntax-parse T1:ow-scheme stx1)
  (define/syntax-parse T2:ow-scheme stx2)

  (define CPARAMs1 (syntax->list #'T1.CPARAMS))
  (define CPARAMs2 (syntax->list #'T2.CPARAMS))

  (and
   ;; Same owner
   (bound-id=? #'T1.OWNER #'T2.OWNER)
   ;; Same number of context parameters
   (eq? (length CPARAMs1) (length CPARAMs2))
   ;; Same context parameters
   (for/and ([CPARAM1 (in-list CPARAMs1)]
             [CPARAM2 (in-list CPARAMs2)])
     (bound-id=? CPARAM1 CPARAM2))))

;; Static Visibility.
;;
;; Ensure statically that `E` refers to a `this` from the class
;; `OT.TYPE`.
;;
;; (: visible? (Syntax OW-SCHEME -> Boolean))
(define (visible? E OT)
  (match-define (list ot.TYPE ot.OWNER ot.CPARAMS)
    (strip-ow-scheme OT))

  ;; All context parameters of `OT`
  (define ctx (cons ot.OWNER (syntax->list ot.CPARAMS)))

  ;; Is the current expression a `this`?
  (define-rules is-this?
    [this #t]
    [ID:id #f]
    [(new ~! _) #f]
    ;; Quid `get-field` return this?
    ;; TODO: check `E` of FNAME is `this`
    ;; And FS(E.ot.TYPE . FNAME).TYPE = t-field.TYPE
    ;;;; I've got the feeling that the only way for such stuff is true
    ;;;; is with `E` is `this`.
    [(get-field ~! E FNAME) #f]
    ;; Quid `set-field` return this?
    [(set-field! ~! E FNAME BODY) #f]
    ;; Quid `send` returns this?
    [(send ~! E DNAME PARAM ...) #f]
    ;; Let is `this` if the expressions returned by the let is `this`.
    ;; TODO: quid it returns a variable bind to `this`?
    [(let ~! (VAR-NAME VAR-SCHEME E) BODY ... LEB) (is-this? #'LEB)]
    ;; By default it is false
    [_ #f])

  ;; A ref is always visible to this. But only non `rep` references
  ;; could be visible to other expression.
  (if (is-this? E) #t
      (not (member #'rep ctx bound-id=?))))

(module+ test
  (define-test-suite utils
    (check-stx=? (get-τ (add-τ #'(Foo o ()) #'(Bar o (n m)))) #'((Foo o ()) (Bar o (n m))))
    (check-stx=? (get-τ (add-τ (add-τ #'(Foo o ()) #'(Bar o (n m))) #'(Baz p (q r))))
                 #'((Foo o ()) (Baz p (q r))))
    (check-stx=? (get-τ #'(Foo o ())) #'((Foo o ()) #f))

    ;; type equality
    (check-true  (τ=? #'(Foo o ())    #'(Foo o ())))
    (check-true  (τ=? #'(Foo o (n))   #'(Foo o (n))))
    (check-true  (τ=? #'(Foo o (n m)) #'(Foo o (n m))))
    (check-true  (τ=? #'(Foo o (n m)) #'(Foo o (n m))))
    (check-true  (τ=? #'(Foo o (n m)) #'(Bar o (n m)))
                 "OT checking only cares of owner and context parameters")
    (check-false (τ=? #'(Foo o (n m)) #'(Foo t (n m)))
                 "OT checks same owner")
    (check-false (τ=? #'(Foo o (n m)) #'(Foo o ()))
                 "OT checks same number of context parameters")
    (check-false (τ=? #'(Foo o (n m)) #'(Foo o (n n)))
                 "OT checks same context parameters")
    (check-false (τ=? #'(Foo o (n m)) #'(Foo o (m n)))
                 "OT checks same context parameters")

    ;; Visibility
    (check-true  (visible? #'_ #'(Foo o (n m))) "Foo is not rep so it is always visible")
    (check-false (visible? #'_ #'(Foo rep (n m))) "Foo is rep so is not visible by default")
    (check-true  (visible? #'this #'(Foo rep (n m))) "rep is visible by this")
    ))


;; Tests

(module+ test
  (require rackunit/text-ui
           (prefix-in env: (submod "env.rkt" ownership test)))
  (provide ownership-tests)

  #;(define-check (check-τ stx b-type)
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

  (define ownership-tests
    (test-suite
     "Ownership checking phase"
     ;; Check env
     env:Σ-tests
     env:Γ-tests
     env:OWS-tests
     env:FS-tests
     env:DS-tests
     ;; Check utils
     utils
     ;; Check phase rules
     ⊢τ-parse
     ;; ⊢e-parse
     ⊢m-parse
     ;; ⊢d-parse
     ))

  (run-tests ownership-tests)
  )


;; Bibliography
;;
;; @InProceedings{CPN98,
;;   author    = {David G. Clarke and
;;                John Potter and
;;                James Noble},
;;   title     = {Ownership Types for Flexible Alias Protection},
;;   booktitle = {Proceedings of the 1998 {ACM} {SIGPLAN} Conference on Object-Oriented
;;                Programming Systems, Languages {\&} Applications {(OOPSLA} '98),
;;                Vancouver, British Columbia, Canada, October 18-22, 1998.},
;;   pages     = {48--64},
;;   year      = {1998},
;;   doi       = {10.1145/286936.286947}
;;   url       = {https://doi.org/10.1145/286936.286947},
;; }
;;
;; @Article{IPW01,
;;   author =       {Atsushi Igarashi and Benjamin C. Pierce and Philip
;;                           Wadler},
;;   title =        {Featherweight Java: a minimal core calculus for Java
;;                                 and GJ},
;;   journal =      {ACM Trans. Program. Lang. Syst.},
;;   volume =       23,
;;   number =       3,
;;   year =         2001,
;;   pages =        {396-450},
;;   ee =           {http://doi.acm.org/10.1145/503502.503505},
;;   bibsource =    {DBLP, http://dblp.uni-trier.de}
;; }
