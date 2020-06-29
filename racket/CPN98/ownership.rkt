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
      #:apply? [env:OWS-arity env:OWS-ref env:ψ])

  ;; Map of fields
  ;;
  ;; (: FS ((Syntaxof (Pairof B-TYPE        ; Class type
  ;;                          Identifier))  ; Field name
  ;;        ~> OW-SCHEME))                  ; Field return type
  (FS #:init meta:FS
      #:mk env:make-FS
      #:apply? [env:FS-member? env:FS-ref])

  ;; Map of definitions
  ;;
  ;; (: DS ((Syntaxof (Pairof Identifier                     ; Class type
  ;;                          Identifier))                   ; Def name
  ;;        ~>
  ;;        (Syntaxof (Pairof (Syntaxof (Listof OW-SCHEME))  ; Type of def args
  ;;                          OW-SCHEME))                    ; Def return type
  (DS #:init (meta-map-w/key
              (λ (kv)
                (match-let* ([`(,DS-key . ,RET-OWS) kv]
                             [`(,CTYPE ,DNAME ,ARG-OWS...) (syntax-e DS-key)])
                  (cons #`(#,CTYPE . #,DNAME) #`(#,ARG-OWS... #,RET-OWS))))
              meta:DS)
      #:mk env:make-DS
      #:apply? [env:DS-member? env:DS-ref])

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
    (with-Σ #'(Θ o n m)
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
   ;; P,Σ,Γ ⊢e (get-field E FNAME) : σ(t-field)
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
   #:with [(ΘPARAM t-param) ...] (stx-map (∘ get-τ ⊢e) #'(PARAM ...))
   #:when (stx-for/and ([t-param #'(t-param ...)]
                        [t-arg   #'(t-arg ...)]
                        [param   #'(ΘPARAM ...)])
            (or (τ=? t-param (σ t-arg))
                (raise (mk-exn:ownership-mismatch t-param (σ t-arg) param))))
   ;; Check SV(E, t-param) ...
   ;;;; Are arguments pass to `DNAME` visible to `E`? An argument to
   ;;;; `DNAME` with a `rep` owner entails that such a argument can
   ;;;; only by accessed form the context of the `t-e.TYPE`
   ;;;; class. Therefore, we have to ensure that `E` in actually in
   ;;;; this context.
   ;;;;
   ;;;; To understand this check, imagine I have a method `def` that
   ;;;; takes one argument of type `rep/Foo`.  Going with such
   ;;;; argument is a specification that intuitively says that
   ;;;; "calling `def` implies to pass an argument from the context of
   ;;;; the current instance".  I can built that argument using the
   ;;;; expression `(new rep/Foo)`.  But, I have no clue in which
   ;;;; context I build that argument.  To get in which context I
   ;;;; build that argument, I have to look at the caller expression
   ;;;; `E`.  If `E` is `this`, for instance, then my `(new rep/Foo)`
   ;;;; is also from `this` that is the context of the current
   ;;;; instance.  However, if `E` is something else, then my `(new
   ;;;; rep/Foo)` is built in another context henceforth results in a
   ;;;; visibility error.
   #:when (stx-for/and ([t-arg #'(t-arg ...)])
            (or (visible? #'E t-arg)
                (raise (mk-exn:visibility-error #'E t-arg))))
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
   #:with [_ ... LEB] (with-Γ (Γ-add #'(VAR-NAME . VAR-SCHEME))
                         (stx-map ⊢e #'(BODY ...)))
   #:with [_ t-leb] (get-τ #'LEB)
   ;; ------------------------------------------------------------------
   ;; P,Γ ⊢e let (VAR-NAME VAR-OW-SCHEME E) BODY ... LEB : t-leb
   (add-τ this-syntax #'t-leb)])

(module+ test
  (define-test-suite ⊢e-parse
    ;; P,Σ,Γ ⊢e E : t
    (with-OWS (list (cons #'Foo #'(Foo Θ ())) (cons #'Bar #'(Bar Θ (ν μ))))
    (with-FS  (list (cons #'(Foo . rep/foo) #'(Foo rep ()))
                    (cons #'(Foo . Θ/foo)   #'(Foo Θ ()))
                    (cons #'(Bar . bar) #'(Bar ν (μ μ))))
    (with-DS  (list (cons #'(Foo . rep-world/def/1) #'([(Foo rep ())] (Foo world ())))
                    (cons #'(Foo . world-rep/def/1) #'([(Foo world ())] (Foo rep ())))
                    (cons #'(Bar . def/0) #'([] (Foo rep ())))
                    (cons #'(Bar . def/2) #'([(Foo ν ()) (Bar ν (μ Θ))] (Bar world (ν ν)))))
    (with-Σ #'(n m o Θ)
    (with-Γ #'{ (this . (Foo Θ ())) }

      ;; [New]             (new ~! SCHEME:ow-scheme)
      ;; Check P,Σ ⊢τ SCHEME-TYPE
      (check-exn exn:arity-error? (thunk (⊢e #'(new (Foo world (world))))))
      (check-exn exn:unknown-cparam? (thunk (⊢e #'(new (Foo z ())))))
      ;; P,Σ,Γ ⊢e (new t) : t
      (check-τ (⊢e #'(new (Foo o ()))) #'(Foo o ()))
      (check-τ (⊢e #'(new (Bar o (n m)))) #'(Bar o (n m)))
      ;; `rep` and `world` are valid universal context parameters
      ;; (universal in the sense that they don't have to be part of
      ;; Σ).
      (check-τ (⊢e #'(new (Foo rep ())))   #'(Foo rep ()))
      (check-τ (⊢e #'(new (Foo world ()))) #'(Foo world ()))

      ;; [Local Access]    ID
      ;; Check ID ∈ dom(Γ)
      (check-not-exn (thunk (⊢e #'this)))
      (check-exn exn:fail:syntax? (thunk (⊢e #'baz)))
      ;; P,Σ,Γ ⊢e ID : Γ(ID)
      (check-τ (⊢e #'this) #'(Foo Θ ()))

      ;; [Field Access]    (get-field ~! E FNAME)
      ;; Check SV(E, t-field)
      (check-exn exn:visibility-error?
                 (thunk (⊢e #'(get-field (new (Foo o ())) rep/foo))))
      (check-exn exn:visibility-error?
                 (thunk (⊢e #'(get-field (new (Foo world ())) rep/foo))))
      ;;;; `rep` of `(new (Foo rep ()))` symbolize the root program
      ;;;; and so it different from the rep is the type of `rep/foo`
      ;;;; which symbolize the instance of Foo. It is quite logic then
      ;;;; that this expression raises a visibility error: The root
      ;;;; program is not allowed to access an inner information of an
      ;;;; instance of Foo.
      (check-exn exn:visibility-error?
                 (thunk (⊢e #'(get-field (new (Foo rep ())) rep/foo))))
      (check-not-exn (thunk (⊢e #'(get-field this rep/foo))))
      ;; P,Σ,Γ ⊢e (get-field E FNAME) : σ(t-field)
      ;;;; According to FS, class Foo has one field Θ/foo of type
      ;;;; Θ/Bar{n m}. Therefore, Θ is supposed to be substituted by
      ;;;; Foo owner. Generally speaking, Θ is always substituted by
      ;;;; the owner of caller expression.
      ;;;; >  Θ/Foo{}::rep/foo is rep/Foo{}
      ;;;; >  Θ/Foo{}::Θ/foo is Θ/Foo{}
      (check-τ (⊢e #'(get-field this rep/foo)) #'(Foo rep ()))
      (check-τ (⊢e #'(get-field this Θ/foo))   #'(Foo Θ ()))
      (check-τ (⊢e #'(get-field (new (Foo o     ())) Θ/foo)) #'(Foo o ()))
      (check-τ (⊢e #'(get-field (new (Foo rep   ())) Θ/foo)) #'(Foo rep ()))
      (check-τ (⊢e #'(get-field (new (Foo world ())) Θ/foo)) #'(Foo world ()))
      ;;;; Θ/Bar{ν μ}::bar is ν/Bar{μ μ}
      (check-τ (⊢e #'(get-field (new (Bar o (rep world))) bar)) #'(Bar rep (world world)))
      (check-τ (⊢e #'(get-field (new (Bar rep (o n))) bar))     #'(Bar o (n n)))

      ;; [Field Update]    (set-field! ~! E FNAME BODY)
      ;; Check P,Σ,Γ ⊢e BODY : σ(t-field)
      (check-exn exn:ownership-mismatch?
                 (thunk (⊢e #'(set-field! (new (Foo rep ())) Θ/foo
                                          (new (Foo world ()))))))
      (check-exn exn:ownership-mismatch?
                 (thunk (⊢e #'(set-field! (new (Foo o ())) Θ/foo
                                          (new (Foo m ()))))))
      (check-exn exn:ownership-mismatch?
                 (thunk (⊢e #'(set-field! (new (Bar rep (o n))) bar
                                          (new (Bar o (n o)))))))
      ;; Check SV(E, t-field)
      (check-exn exn:visibility-error?
                 (thunk (⊢e #'(set-field! (new (Foo rep ())) rep/foo
                                          (new (Foo rep ()))))))
      (check-not-exn (thunk (⊢e #'(set-field! this rep/foo (get-field this rep/foo)))))
      (check-not-exn (thunk (⊢e #'(set-field! this rep/foo (get-field (new (Foo rep ())) Θ/foo)))))
      ;; P,Σ,Γ ⊢e (set-field (E : t-e) FNAME BODY) : σ(t-field)
      (check-τ (⊢e #'(set-field! this rep/foo (get-field this rep/foo))) #'(Foo rep ()))
      (check-τ (⊢e #'(set-field! this Θ/foo this))                       #'(Foo Θ ()))
      (check-τ (⊢e #'(set-field! (new (Foo o ())) Θ/foo (new (Foo o ())))) #'(Foo o ()))
      (check-τ (⊢e #'(set-field! (new (Foo rep ())) Θ/foo (new (Foo rep ())))) #'(Foo rep ()))
      (check-τ (⊢e #'(set-field! (new (Bar o (rep world))) bar (new (Bar rep (world world)))))
               #'(Bar rep (world world)))

      ;; [Method Call]     (send ~! E DNAME PARAM ...)
      (with-syntax ([new-bar #'(new (Bar o (rep world)))]
                    [rep/arg #'(new (Foo rep ()))]
                    [world/arg #'(new (Foo world ()))])
        ;; Check P,Σ,Γ ⊢e E t-e
        ;; Check P,Σ,Γ ⊢e (PARAM : σ(t-arg)) ...
        ;;;; `new-bar` is of type `o/Bar{rep world}`. In this context,
        ;;;; `def/2` is of type
        ;;;; (: rep/Foo rep/Bar{world o} -> world/Bar{rep rep})
        (check-exn exn:ownership-mismatch?
                   (thunk (⊢e #'(send new-bar def/2
                                      (new (Foo n ()))
                                      (new (Bar rep (world o))))))
                   "`n/Foo` mismatches with the expected `rep/Foo`")
        (check-exn exn:ownership-mismatch?
                   (thunk (⊢e #'(send new-bar def/2
                                      (new (Foo rep ()))
                                      (new (Bar n (world o))))))
                   "`n/Bar{world o}` mismatches with the expected `rep/Bar{world o}`")
        (check-exn exn:ownership-mismatch?
                   (thunk (⊢e #'(send new-bar def/2
                                      (new (Foo rep ()))
                                      (new (Bar rep (n o))))))
                   "`rep/Bar{n o}` mismatches with the expected `rep/Bar{world o}`")
        (check-exn exn:ownership-mismatch?
                   (thunk (⊢e #'(send new-bar def/2
                                      (new (Foo rep ()))
                                      (new (Bar rep (world n))))))
                   "`rep/Bar{world n}` mismatches with the expected `rep/Bar{world o}`")
        ;; Check SV(E, t-param) ...
        ;;;; rep-world/def/1 takes a rep/Foo argument (with rep means
        ;;;; something in the context of Foo), but here the rep comes
        ;;;; from the caller `(new (Foo rep ()))` that references
        ;;;; root...  Therefore, the caller is not supposed to be
        ;;;; allowed to pass arguments to that method.
        (check-exn exn:visibility-error?
                   (thunk (⊢e #'(send (new (Foo rep ())) rep-world/def/1 rep/arg))))
        ;; Check SV(E, t-field)
        ;;;; world-rep/def/1 returns a rep/Foo value (with rep means
        ;;;; something in the context of Foo and is supposed to not go
        ;;;; out of that context).  But here, I tried to access it using
        ;;;; the expression `(new (Foo rep ()))` which refers to the
        ;;;; context of root.  Therefore the caller is not supposed to
        ;;;; be allowed to access this value.
        (check-exn exn:visibility-error?
                   (thunk (⊢e #'(send (new (Foo rep ())) world-rep/def/1 world/arg))))
        ;; Check P,Σ,Γ ⊢e (send (E : t) DNAME PARAM ...) : σ(t-ret)
        (check-τ (⊢e #'(send new-bar def/2
                             (new (Foo rep ()))
                             (new (Bar rep (world o)))))
                 #'(Bar world {rep rep})
                 "(: rep/Foo rep/Bar{world o} -> world/Bar{rep rep})")
        (check-τ (⊢e #'(send this rep-world/def/1 rep/arg))
                 #'(Foo world {})
                 "(: rep/Foo -> world/Foo)")
        (check-τ (⊢e #'(send this world-rep/def/1 world/arg))
                 #'(Foo rep {})
                 "(: world/Foo -> rep/Foo)"))

      ;; [Local Update, Sequence]  (let ~! (VAR-NAME VAR-SCHEME E) BODY ...)
      ;; Check P,Σ ⊢τ VAR-SCHEME
      (check-exn exn:arity-error?
                 (thunk (⊢e #'(let (foo (Foo world {world}) _) _)))
                 "type Foo does not have context parameters")
      (check-exn exn:unknown-cparam?
                 (thunk (⊢e #'(let (foo (Foo z {}) _) _)))
                 "Context parameter `z` is not part of Σ")
      ;; Check  P,Σ,Γ ⊢e E : VAR-SCHEME  (τ=?)
      (check-exn exn:ownership-mismatch?
                 (thunk (⊢e #'(let (foo (Foo rep {}) (new (Foo world {}))) _)))
                 "foo expects a rep/Foo but a world/Foo was given")
      (check-exn exn:ownership-mismatch?
                 (thunk (⊢e #'(let (bar (Bar o {n m}) (new (Bar o {m m}))) _)))
                 "bar expects a o/Bar{n m} but a o/Bar{m m} was given")
      (check-exn exn:ownership-mismatch?
                 (thunk (⊢e #'(let (bar (Bar o {n m}) (new (Bar o {n n}))) _)))
                 "bar expects a o/Bar{n m} but a o/Bar{n n} was given")
      ;; Check P,Σ,{VAR-NAME: VAR-SCHEME, ...} ⊢e E ... LEB : t-leb
      (check-not-exn (thunk (⊢e #'(let (foo (Foo Θ {}) this) foo))))
      (check-not-exn (thunk (⊢e #'(let (foo (Foo Θ {}) ???) foo)))
                     "A let binding accepts the `???` as expression")
      (check-not-exn (thunk (⊢e #'(let (foo (Foo rep {}) ???) foo)))
                     "A let binding accepts the `???` as expression")
      ;; FIXME:
      ;; (check-not-exn (thunk (⊢e #'(let (bind-this (Foo Θ {}) this)
      ;;                               (get-field bind-this rep/foo))))
      ;;                "Binding this is still this")
      ;; P,Γ ⊢e let (VAR-NAME VAR-OW-SCHEME E) BODY ... LEB : t-leb
      (check-τ (⊢e #'(let (foo (Foo rep {}) ???) foo))  #'(Foo rep {}))
      (check-τ (⊢e #'(let (foo (Foo rep {}) ???) this)) #'(Foo Θ {}))
      (check-τ (⊢e #'(let (foo (Foo rep {}) ???) this foo)) #'(Foo rep {}))
      (check-τ (⊢e #'(let (foo (Foo rep {}) ???) foo this)) #'(Foo Θ {}))
      (check-τ (⊢e #'(let (foo (Foo rep {}) ???)
                       (let (foo (Bar o {n m}) ???) foo)))
               #'(Bar o {n m})
               "Binding of inner let shadows the binding of outer let")

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
;; `OT.TYPE`.  This implementation follows the definition of CPN98,
;; but it has few flaws.  See tests for `visible?` below.
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
    ;; FIXME:
    ;; (check-true  (visible? #'(let (binder (Foo Θ (n m)) this) binder) #'(Foo rep (n m)))
    ;;              "binding this is this")
    ))


;; Tests

(module+ test
  (require rackunit/text-ui
           (prefix-in env: (submod "env.rkt" ownership test)))
  (provide ownership-tests)

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

      (thunk (with-handlers ([exn:arity-error? fail]
                             [exn:unknown-cparam? fail])
               (check-true (τ=? stx-type b-type))))))

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
     ⊢e-parse
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
