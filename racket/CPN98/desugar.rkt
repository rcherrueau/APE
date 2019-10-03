#lang racket/base

;; @InProceedings{CPN98,
;;   author    = {David G. Clarke and
;;                John Potter and
;;                James Noble},
;;   title     = {Ownership Types for Flexible Alias Protection},
;;   booktitle = {Proceedings of the 1998 {ACM} {SIGPLAN} Conference
;;                on Object-Oriented Programming Systems, Languages
;;                {\&} Applications {(OOPSLA} '98),
;;                Vancouver, British Columbia, Canada, October 18-22, 1998.},
;;   pages     = {48--64},
;;   year      = {1998},
;;   url       = {https://doi.org/10.1145/286936.286947},
;;   doi       = {10.1145/286936.286947}
;; }

(require racket/match
         racket/function
         racket/list
         racket/pretty
         racket/syntax
         "utils.rkt"
         "definitions.rkt"
         syntax/parse
         syntax/parse/define
         syntax/module-reader
         syntax/stx
         (for-syntax syntax/parse
                     syntax/parse/define
                     racket/base
                     "definitions.rkt"
                     )
 )

(provide ∗>)


;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects
;; - c-type>, binder> Introduce class type/binder for the rest of thread
;; - c-type+, binder+ Mark class type/binder of the current syntax


;; Desugaring syntax transformation (∗>)

;; - Introduce missing CPARAM
;; - Transform let with multiple binding into nested lets of one
;;   binding.
;; - Rename short field access to normal (get-field this field)
;;   access.
;; - Mark fields with the name of their class
;; - Mark free identifiers with their binder
;; - Expand types to ownership schemes

;; ∗> :: stx -> stx
(define ∗> (syntax-parser
  #:literal-sets [keyword-lits expr-lits arg-lits]

  ;; A prog is a list of CLASS and one expression E.
  ;;
  ;; (prog CLASS ... E)
  ;; ∗>  (*prog *CLASS ... *E)
  ;;
  ;; The `~!` eliminate backtracking. Hence, if the next
  ;; `fail-when` failed, it will not backtrack and reach the `_`
  ;; case.
  [(prog ~! CLASS:expr ... E:expr)
   #:with [*CLASS ...] (stx-map ∗> #'(CLASS ...))
   #:with *E           (∗> #'E)
   #'(*prog *CLASS ... *E)]

  ;; A class is a NAME, an optional list of context parameters
  ;; CPARAM, and a list of fields and definitions.
  ;;
  ;; (class NAME (CPARAM ...)? FIELD ... DEF ...)
  ;; ∗>  (*class NAME *FIELD ... *DEF ...)
  [(class NAME:id [CPARAM:id ...] ~! FIELD/DEF:expr ...)
   #:with [*FIELD/DEF ...]     (stx-map (#'NAME . c-type> . ∗>)
                                        #'(FIELD/DEF ...))
   #'(*class NAME [CPARAM ...] *FIELD/DEF ...)]
  ;; Transforms a `class` without `CPARAM ...` into a `class` with.
  [(class NAME FIELD/DEF ...)
   (∗> #'(class NAME [] FIELD/DEF ...))]

  ;; A field declares one argument ARG (i.e., no initialization).
  ;;
  ;; (field ARG)
  ;; ∗>  (*field NAME . OW-SCHEME)
  ;; with OW-SCHEME is OWNER, TYPE, CPARAMS
  [(field ARG:arg)
   #:with NAME      (c-type+ #'ARG.NAME)
   #:with OW-SCHEME (type∗>ownership-scheme #'ARG.T)
   #'(*field NAME . OW-SCHEME)]

  ;; A def (i.e., method) is a NAME, a list of arguments ARG, a
  ;; return type RET and the BODY of the def. The def binds
  ;; ARG in the BODY. The binding relation is noted
  ;;`(ARG ...) binder*>`.
  ;;
  ;; (def (NAME ARG ... → RET) BODY)
  ;; ∗>  (*def (NAME (A-NAME . A-OW-SCHEME) ... RET-OW-SCHEME) *BODY)
  ;; with OW-SCHEME is OWNER, TYPE, CPARAMS
  [(def (NAME:id ARG:arg ... → RET:type) BODY:expr)
   #:with *BODY             ((#'(ARG ...) . binder> . ∗>) #'BODY)
   #:with [A-NAME ...]      #'(ARG.NAME ...)
   #:with [A-OW-SCHEME ...] (stx-map type∗>ownership-scheme #'(ARG.T ...))
   #:with RET-OW-SCHEME     (type∗>ownership-scheme #'RET)
   #'(*def (NAME (~@ (A-NAME . A-OW-SCHEME)) ... RET-OW-SCHEME) *BODY)]

  ;; A let binds a variables VAR with a type T to an expression E in a
  ;; BODY. The binding relation is noted `binder> VAR`.
  ;;
  ;; (let ([VAR : T E] ...) BODY)
  ;; ∗>  (*let (VAR OW-SCHEME *E) (*let... (...) *BODY)
  ;; with OW-SCHEME is OWNER, TYPE, CPARAMS
  [(let ([VAR:id : T:type E:expr]) ~! BODY:expr)
   #:with (OW-SCHEME-VAL ...) (type∗>ownership-scheme #'T)
   #:with *E                  (∗> #'E)
   #:with *BODY               ((#'VAR . binder> . ∗>) #'BODY)
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
  [(new C-TYPE:type)
   #:with OW-SCHEME (type∗>ownership-scheme #'C-TYPE)
   #'(*new . OW-SCHEME)]

  ;; A get-field takes an expression E that should reduce to an
  ;; object and the name of the field FNAME to get on that object.
  ;;
  ;; (get-field E FNAME)
  ;; ∗>  (*get-field *E FNAME)
  [(get-field E:expr FNAME:id)
   #:with *E (∗> #'E)
   #'(*get-field *E FNAME)]

  ;; A set-field! takes an expression E that should reduce to an
  ;; object, the name of the field FNAME to change the value of, and
  ;; the BODY of the new value.
  ;;
  ;; (set-field! E FNAME BODY)
  ;; ∗>  (*set-field! *E FNAME *BODY)
  [(set-field! E:expr FNAME:id BODY:expr)
   #:with *E    (∗> #'E)
   #:with *BODY (∗> #'BODY)
   #'(*set-field! *E FNAME *BODY)]

  ;; A send takes an expression E that should reduce to an object,
  ;; the name of the def DNAME to call on that object, and a list of
  ;; expressions `E-ARG ...` to pass as arguments to the def.
  ;;
  ;; (send E DNAME E-ARG ...)
  ;; ∗>  (*send *E DNAME *E-ARG)
  [(send E:expr DNAME:id E-ARG:expr ...)
   #:with *E           (∗> #'E)
   #:with [*E-ARG ...] (stx-map ∗> #'(E-ARG ...))
   #'(*send *E DNAME *E-ARG ...)]

  ;; An identifier is either:
  ;;
  ;; - The reserved keyword `this`. Marked with the current class
  ;;   type.
  [this
   (c-type+ #'*this)]
  ;; - A local binding (from a def or let). Marked with its binder.
  [ID:id #:when (is-locally-binded? #'ID)
    (binder+ #'ID)]
  ;; - A class level binding (no binder). In that case, it presumably
  ;;   refers to a field of the current class: A sort of shortcut for
  ;;   (get-field this id) -- i.e., `id` instead of `this.id` in terms
  ;;   of Java. E.g.,
  ;;
  ;;   1 (class C
  ;;   2   (field [id : A])
  ;;   3   (def (get-id → A) id))
  ;;
  ;;   With line 3 a shortcut for
  ;;   > (def (get-id → A) (get-field this id))
  ;;
  ;;   We remove it, so the desugared syntax contains no class level
  ;;   binding.
  ;;   ID ∗> *(get-field this ID)
  [ID:id
   ;; #:with *ID (c-type+ #'ID)
   (∗> #'(get-field this ID))]))


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


;; (provide (rename-out
;;           [surface-read read]
;;           [surface-read-syntax read-syntax]))

(define (type∗>ownership-scheme stx)
  (syntax-parse stx
    [T:type #'(T.OWNER T.TYPE T.CPARAMS)]
    [raise-syntax-error #f "Not an ownership type" stx]))


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
