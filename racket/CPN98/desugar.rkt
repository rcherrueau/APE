#lang racket/base

(require (for-syntax racket/base
                     "definitions.rkt"
                     )
         racket/performance-hint
         syntax/parse
         syntax/parse/define
         syntax/stx
         "utils.rkt"
         "definitions.rkt"
 )

(provide ∗>)

;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects
;; - c-type>, binder> Introduce class type/binder for the rest of thread
;; - c-type+, binder+ Mark class type/binder of the current syntax


;; Desugaring syntax transformation (∗>)
;;
;; - Introduce missing CPARAM
;; - Transform let with multiple binding into nested lets of one
;;   binding.
;; - Rename short field access to normal (get-field this field)
;;   access.
;; - Mark fields with the name of their class
;; - Mark free identifiers with their binder
;; - Expand types to ownership schemes
;;
;; ∗> :: stx -> stx
(define-parser ∗>
  #:literal-sets [keyword-lits expr-lits type-lits]

  ;; A prog is a list of CLASS and one expression E.
  ;;
  ;; (prog CLASS ... E)
  ;; ∗>  (*prog *CLASS ... *E)
  ;;
  ;; Note: The `~!` eliminate backtracking. Hence, if the next
  ;; `fail-when` failed, it will not backtrack and try other cases.
  [(prog ~! CLASS:expr ... E:expr)
   #:with [*CLASS ...] (stx-map ∗> #'(CLASS ...))
   #:with *E           (∗> #'E)
   #'(*prog *CLASS ... *E)]

  ;; A class is a NAME, an optional list of context parameters
  ;; CPARAM, and a list of fields and definitions.
  ;;
  ;; (class NAME (CPARAM ...)? FIELD ... DEF ...)
  ;; ∗>  (*class NAME (CPARAM ...) *FIELD ... *DEF ...)
  [(class NAME:id [CPARAM:id ...] ~! FIELD/DEF:expr ...)
   #:with [*FIELD/DEF ...] (stx-map (#'NAME . c-type> . ∗>) #'(FIELD/DEF ...))
   #'(*class NAME [CPARAM ...] *FIELD/DEF ...)]
  ;; Transforms a `class` without `CPARAM ...` into a `class` with.
  [(class NAME FIELD/DEF ...)
   (∗> #'(class NAME [] FIELD/DEF ...))]

  ;; A field declares one argument ARG (i.e., no initialization).
  ;;
  ;; (field ARG)
  ;; ∗>  (*field NAME . OW-SCHEME)
  ;; with OW-SCHEME is OWNER, TYPE, CPARAMS
  [(field ~! ARG:arg)
   #:with NAME      (c-type+ #'ARG.NAME)
   #:with OW-SCHEME (type∗>ow-scheme #'ARG.T)
   #'(*field NAME . OW-SCHEME)]

  ;; A def (i.e., method) is a NAME, a list of arguments ARG, a return
  ;;type RET and the BODY of the def. The def binds ARG in the BODY.
  ;;The binding relation is noted `(ARG ...) binder*>`. It puts `(ARG
  ;;...)` in the binder context of ∗>.
  ;;
  ;; (def (NAME ARG ... → RET) BODY)
  ;; ∗>  (*def (NAME (A-NAME . A-OW-SCHEME) ... RET-OW-SCHEME) *BODY)
  ;; with OW-SCHEME is OWNER, TYPE, CPARAMS
  [(def ~! (NAME:id ARG:arg ... → RET:type) BODY:expr)
   #:with *BODY             ((#'(ARG ...) . binder> . ∗>) #'BODY)
   #:with [A-NAME ...]      #'(ARG.NAME ...)
   #:with [A-OW-SCHEME ...] (stx-map type∗>ow-scheme #'(ARG.T ...))
   #:with RET-OW-SCHEME     (type∗>ow-scheme #'RET)
   #'(*def (NAME (~@ (A-NAME . A-OW-SCHEME)) ... RET-OW-SCHEME) *BODY)]

  ;; A let binds a variables VAR with a type T to an expression E in a
  ;; BODY. The binding relation is noted `binder> VAR`. It puts VAR in
  ;; the binder context of ∗>.
  ;;
  ;; (let ([VAR : T E] ...) BODY)
  ;; ∗>  (*let (VAR OW-SCHEME *E) (*let... (...) *BODY)
  ;; with OW-SCHEME is OWNER, TYPE, CPARAMS
  [(let ([VAR:id : T:type E:expr]) ~! BODY:expr)
   #:with (OW-SCHEME-VAL ...) (type∗>ow-scheme #'T)
   #:with *E                  (∗> #'E)
   #:with *BODY               ((#'(VAR : T) . binder> . ∗>) #'BODY)
   #'(*let (VAR OW-SCHEME-VAL ... *E) *BODY)]
  ;; Transforms a `let` with multiple binding into multiple nested
  ;; `let`s with one unique binding (such as the previous let)
  [(let (B1 BS ...) BODY:expr)
   (∗> #'(let (B1) (let (BS ...) BODY)))]

  ;; A new takes the class type C-TYPE of the class to instantiate
  ;; (i.e., no constructor).
  ;;
  ;; (new C-TYPE)
  ;; ∗>  (*new OW-SCHEME)
  [(new ~! C-TYPE:type)
   #:with OW-SCHEME (type∗>ow-scheme #'C-TYPE)
   #'(*new . OW-SCHEME)]

  ;; A get-field takes an expression E that should reduce to an
  ;; object and the name of the field FNAME to get on that object.
  ;;
  ;; (get-field E FNAME)
  ;; ∗>  (*get-field *E FNAME)
  [(get-field ~! E:expr FNAME:id)
   #:with *E (∗> #'E)
   #'(*get-field *E FNAME)]

  ;; A set-field! takes an expression E that should reduce to an
  ;; object, the name of the field FNAME to change the value of, and
  ;; the BODY of the new value.
  ;;
  ;; (set-field! E FNAME BODY)
  ;; ∗>  (*set-field! *E FNAME *BODY)
  [(set-field! ~! E:expr FNAME:id BODY:expr)
   #:with *E    (∗> #'E)
   #:with *BODY (∗> #'BODY)
   #'(*set-field! *E FNAME *BODY)]

  ;; A send takes an expression E that should reduce to an object,
  ;; the name of the def DNAME to call on that object, and a list of
  ;; expressions `E-ARG ...` to pass as arguments to the def.
  ;;
  ;; (send E DNAME E-ARG ...)
  ;; ∗>  (*send *E DNAME *E-ARG)
  [(send ~! E:expr DNAME:id E-ARG:expr ...)
   #:with *E           (∗> #'E)
   #:with [*E-ARG ...] (stx-map ∗> #'(E-ARG ...))
   #'(*send *E DNAME *E-ARG ...)]

  ;; An identifier is either:
  ;;
  ;; 1. The reserved keyword `this`. Marked with the current class
  ;;    type.
  [this
   (c-type+ #'*this)]
  ;; 2. A local binding (from a def or let). Marked with its binder.
  [ID:id #:when (is-locally-binded? #'ID)
    (binder+ #'ID)]
  ;; 3. A class level binding (no binder). In that case, it presumably
  ;;    refers to a field of the current class: A sort of shortcut for
  ;;    (get-field this id) -- i.e., `id` instead of `this.id` in
  ;;    Java world. E.g.,
  ;;
  ;;    1 (class C
  ;;    2   (field [id : A])
  ;;    3   (def (get-id → A) id))
  ;;
  ;;    With line 3 a shortcut for
  ;;    > (def (get-id → A) (get-field this id))
  ;;
  ;;    We remove it, so the desugared syntax contains no class level
  ;;    binding.
  ;;    ID ∗> *(get-field this ID)
  [ID:id
   (∗> #'(get-field this ID))])


;; Lang

;; ;; Syntax parameters, as racket `parameter`, but at transformer level
;; ;; - https://beautifulracket.com/explainer/parameters.html#syntax-parameters
;; ;; - https://www.greghendershott.com/fear-of-macros/Syntax_parameters.html
;; ;;
;; ;; These parameters can be accessed inside a syntax object and give
;; ;; the binded stx object in return. E.g., in the following,
;; ;; `#'my-param` syntax object is, actually, a binder for the `#'42`
;; ;; syntax object.
;; ;;
;; ;; > (define-syntax-parameter my-param (λ (stx) #'42))
;; ;; > ...
;; ;; > #'(writeln my-param) ; 42
;; ;;
;; ;; And I can later rebind it to another value with
;; ;; `syntax-parameterize`, analogously to `parameterize` .
;; ;;
;; ;; These parameters should take place in a syntax-object for phase 0
;; ;; (runtime). However, in the following, I use these parameters to
;; ;; track information during phase 1 (macro expansion). Thus, I bind
;; ;; them to a syntax error if someone try to use it a runtime.
;; (define-syntax-parameter current-class-type  ;; Syntax object that is
;;                                              ;; the type of the
;;                                              ;; current class.
;;   (λ (stx) (raise-syntax-error #f "Should only be used during macro expansion" stx)))
;;
;; (define-syntax-parameter field-types        ;; Map of `(class-τ . field-name) -> field-τ`
;;   (λ (stx) (raise-syntax-error #f "Should only be used during macro expansion" stx)))

;; (define current-class-type (make-parameter #f))
;; (define local-bindings     (make-parameter #hash{}))


;; (define bind-local (curry bind local-bindings))


;; Expand to:
;; (let* ([arg-name-stxs (syntax-parse #'(ARG ...)
;;                         [(A:arg ...) (syntax->list #'(A.NAME ...))])]
;;        [arg-names (map syntax->datum arg-name-stxs)]
;;        [arg-stxs (syntax->list #'(ARG ...))]
;;        [binders (interleave arg-names arg-stxs)]
;;        [new-bindings (apply hash-set* (local-bindings) binders)])
;;  (parameterize ([local-bindings new-bindings]) (*d #'BODY))))


;; (define-syntax-parser bind*
;;   [(_ BINDERS:expr BODY:expr)
;;    #'(let* ([binder-name-stxs
;;              (syntax-parse BINDERS
;;                [(ARG ...) #'(ARG ...)])]
;;             [binder-names (map syntax->datum binder-name-stxs)]
;;             [binder-stxs (syntax->list BINDERS)]
;;             [binders (interleave binder-names binder-stxs)]
;;             [new-bindings (apply hash-set* (local-bindings) binders)])
;;                   (parameterize ([local-bindings new-bindings]) BODY))
;;    ])

;; #,(let* ([arg-name-stxs (syntax-parse #'(ARG ...)
;;                           [(A:arg ...) (syntax->list #'(A.NAME ...))])]
;;          [arg-names (map syntax->datum arg-name-stxs)]
;;          [arg-stxs (syntax->list #'(ARG ...))]
;;          [binders (interleave arg-names arg-stxs)]
;;          [new-bindings (apply hash-set* (local-bindings) binders)])
;;     (parameterize ([local-bindings new-bindings]) (*d #'BODY)))


;; Syntax for type and arg

(define-literal-set type-lits
  ;; Don't consider :, →, and / as patterns
  #:datum-literals (: → /)
  ())

(define-syntax-class type
  #:description "class type with ownership and context parameters"
  #:literal-sets [type-lits]
  #:attributes [TYPE OWNER CPARAMS]
  (pattern (O:id / T:id)
           #:with OWNER #'O
           #:with TYPE #'T
           #:with CPARAMS #'())
  (pattern (O:id / (T:id PARAMS:id ...+))
           #:with OWNER #'O
           #:with TYPE #'T
           #:with CPARAMS #'(PARAMS ...))
  (pattern T:id
           #:with OWNER #''Θ
           #:with TYPE #'T
           #:with CPARAMS #''())
  (pattern (T:id PARAMS:id ...+)
           #:with OWNER #''Θ
           #:with TYPE #'T
           #:with CPARAMS #'(PARAMS ...)))

(define-syntax-class arg
  #:description "argument with its type"
  #:literal-sets [type-lits]
  (pattern (NAME:id : T:type)
           #:attr OWNER #'T.OWNER
           #:attr TYPE  #'T.TYPE
           #:attr CPARAMS #'T.CPARAMS))

(define (is-locally-binded? stx)
  ;; (writeln (local-bindings))
  (let ([id-name (syntax->datum stx)])
    (hash-has-key? (local-bindings) id-name)))


;; Utils
;; #:with OW-SCHEME (type∗>ow-scheme #'ARG.T)
(define type∗>ow-scheme (syntax-parser
  [T:type (make-*ow-scheme T T.TYPE T.OWNER T.CPARAMS)]))

;; c-type> :: (C-TYPE: stx) -> (t: stx -> stx) -> (stx -> stx)
;; Parameterize the syntax transformer `t` with class type `C-TYPE`.
(define (c-type> C-TYPE t)
  (λ (stx)
    (parameterize ([current-class-type C-TYPE]) (t stx))))

;; binder> :: (BINDERS: stx:arg or [stx:arg]) -> (t: stx -> stx) -> (stx -> stx)
(define (binder> BINDERS t)

  ;; (get-arg-name #'(a : b)) ;; (values 'a #<syntax b>)
  (define (get-arg-name/OW-SCHEME stx)
    (syntax-parse stx
      [ARG:arg (values (syntax->datum #'ARG.NAME)
                       (syntax->list (type∗>ow-scheme #'ARG.TYPE)))]))

  ;; binder1> :: (BINDER: stx) -> (t: stx -> stx) -> (stx -> stx)
  (define (binder1> BINDER t)
    (λ (stx)
      (let*-values ([(binder-name OW-SCHEME) (get-arg-name/OW-SCHEME BINDER)]
                    [(new-bindings) (hash-set (local-bindings) binder-name OW-SCHEME)])
        (parameterize ([local-bindings new-bindings]) (t stx)))))

  (syntax-parse BINDERS
    ;; Match unique arg/id
    [b:id  (binder1> #'b t)]
    [b:arg (binder1> #'b t)]

    ;; Match list of arg/id
    [()         t]
    [(b)        (binder1> #'b t)]
    [(b bs ...) (binder1> #'b (binder> #'(bs ...) t))]))

;; Reader
;; (define (surface-read in)
;;   (syntax->datum (surface-read-syntax #f in)))


;; (define (surface-read-syntax src in)
;;   (define the-s-exps
;;     (let s-exps ([s-exp (read-syntax src in)])
;;       (if (eof-object? s-exp)
;;           '()
;;           (cons s-exp (s-exps (read-syntax src in))))))

;;   (define prog-sexp (quasisyntax/loc (car the-s-exps) (prog #,@the-s-exps)))
;;   (pretty-print (syntax->datum (∗> prog-sexp)))

;;     ;; (define the-s-exp #`(#,@(s-exps)))
;;     ;; (d* #`(prog #,@the-s-exp)))

;;   ;; ;;   (match (read-syntax src in)
;;   ;; ;;     [eof '()]
;;   ;; ;;     [_ (cons sexp (desugar))]
;;   ;; ;;   ))
;;   ;; ;; (letrec ([desugar (λ () (read-syntax src in))])
;;   ;; ;;   ()
;;   ;; ;;   )

;;   ;; ;; TODO: use `read-syntax' of s-exp then parten match on it
;;   ;; (define prog (desugar))
;;   ;; (displayln prog)

;;   ;; #`(module m "surface-desugar-lang.rkt" #,@prog)
;;   ;; (error "lala")
;;   #'(module m racket/base (void))
;;   )

;; See, https://github.com/racket/racket/blob/2b567b4488ff92e2bc9c0fbd32bf7e2442cf89dc/pkgs/at-exp-lib/at-exp/lang/reader.rkt#L15
;; (define-values
;;   (surface-read surface-read-syntax surface-get-info)
;;   (make-meta-reader
;;    'surface-lang
;;    "language path"
;;    lang-reader-module-paths
;;    s-reader
;;    TODO...))
;; Expander
