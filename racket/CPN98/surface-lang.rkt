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
         (for-syntax syntax/parse
                     syntax/parse/define
                     racket/base
                     "definitions.rkt"
                     )
 )


;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects
;; - c-type>, binder> Introduce class-type/binder for the rest of thread
;; - c-type+, binder+ Mark class-type/binder of the current syntax


;; Desugaring syntax transformation (∗>)

;; - Introduce missing CPARAM
;; - Transform let with multiple binding into nested lets of one
;;   binding.
;; - Rename short field access to normal (get-field this field)
;;   access.
;; - Mark fields with the name of their class
;; - Mark free identifiers with their binder
;; - Check no duplicate class name, field, def

;; ∗> :: stx -> stx
(define ∗> (syntax-parser
  #:literal-sets [keyword-lits expr-lits arg-lits]

  ;; A prog is a list of classes and one expression.
  ;;
  ;; (prog CLASS ... E)
  ;; ∗>  (*prog *CLASS ... *E)
  ;;
  ;; The `~!` eliminate backtracking. Hence, if the next
  ;; `fail-when` failed, it will not backtrack and reach the `_`
  ;; case.
  [(prog ~! CLASS:expr ... E:expr)
   #:fail-when (check-class #'(CLASS ...)) "Duplicated class name"
   #:with [*CLASS ...] (map ∗> (syntax->list #'(CLASS ...)))
   #:with *E           (∗> #'E)
   #'(*prog (*CLASS ...) *E)]

  ;; A class is a `name`, an optional list of context parameters
  ;; `CPARAM`, and a list of fields and definitions.
  ;;
  ;; (class name (CPARAM ...)? FIELD ... DEF ...)
  ;; ∗>  (*class name *FIELD ... *DEF ...)
  [(class name:id [CPARAM:id ...] ~! FIELD/DEF:expr ...)
   #:fail-when (field-twice? #'(FIELD/DEF ...)) "Duplicated field"
   #:fail-when (def-twice? #'(FIELD/DEF ...))   "Duplicated def"
   #:with [*FIELD/DEF ...] (map (#'name . c-type> . ∗>)
                                (syntax->list #'(FIELD/DEF ...)))
   #'(*class name [CPARAM ...] (*FIELD/DEF ...))]
  ;; Transforms a `class` without `CPARAM ...` into a `class` with.
  [(class name FIELD/DEF ...)
   (∗> #'(class name [] FIELD/DEF ...))]

  ;; A field declares one argument `ARG` (i.e., no initialization).
  ;;
  ;; (field ARG)
  ;; ∗>  (*field ARG)
  [(field ARG:arg)
   #:with *ARG (c-type+ #'ARG)
   #'(*field *ARG)]

  ;; A def (i.e., method) is a `name`, a list of arguments `ARG`, a
  ;; return type `ret` and the `BODY` of the def. The def binds
  ;; `ARG` in the `BODY`. The binding relation is noted
  ;; `(ARG ...) binder*> `.
  ;;
  ;; (def (name ARG ... → ret) BODY)
  ;; ∗>  (*def (name *ARG ... . ret) *BODY)
  [(def (name:id ARG:arg ... → ret:type) BODY:expr)
   #:with *BODY (((syntax->list #'(ARG ...)) . binder*> . ∗>) #'BODY)
   #'(*def (name ARG ... . ret) *BODY)]

  ;; A let binds a variables `var` to expression `E` in a BODY. The
  ;; binding relation is noted `binder> var`.
  ;;
  ;; (let ([var : type E] ...) BODY)
  ;; ∗>  (*let (var : type *E) (*let... (...) *BODY)
  [(let ([var:id : t:type E:expr]) ~! BODY:expr)
   #:with *E    (∗> #'E)
   #:with *BODY ((#'var . binder> . ∗>) #'BODY)
   #'(*let (var : t *E) *BODY)]
  ;; Transforms a `let` with multiple binding into multiple nested
  ;; `let`s with one unique binding (such as the previous let)
  [(let (B1 BS ...) BODY:expr)
   (∗> #'(let (B1) (let (BS ...) BODY)))]

  ;; A new takes the class type `c-type` of the class to instantiate
  ;; (i.e., no constructor).
  ;;
  ;; (new c-type)
  ;; ∗>  (*new c-type)
  [(new c-type:type)
   #'(*class c-type)]

  ;; A get-field takes an expression `E` that should reduce to an
  ;; object and the name of the field `fname` to get on that object.
  ;;
  ;; (get-field E fname)
  ;; ∗>  (*get-field *E fname)
  [(get-field E:expr fname:id)
   #:with *E (∗> #'E)
   #'(*get-field *E fname)]

  ;; A set-field! takes an expression `E` that should reduce to an
  ;; object, the name of the field `fname` to change the value of, and
  ;; the `BODY` of the new value.
  ;;
  ;; (set-field! E fname BODY)
  ;; ∗>  (*set-field! *E fname *BODY)
  [(set-field! E:expr fname:id BODY:expr)
   #:with *E    (∗> #'E)
   #:with *BODY (∗> #'BODY)
   #'(*set-field! *E fname *BODY)]

  ;; A send takes an expression `E` that should reduce to an object,
  ;; the name of the def `dname` to call on that object, and a list of
  ;; expressions `E-ARG ...` to pass as arguments to the def.
  ;;
  ;; (send E dname E-ARG ...)
  ;; ∗>  (*get-field *E fname *E-ARG)
  [(send E:expr dname:id E-ARG:expr ...)
   #:with *E           (∗> #'E)
   #:with [*E-ARG ...] (map ∗> (syntax->list #'(E-ARG ...)))
   #'(*send *E dname *E-ARG ...)]

  ;; An identifier is either:
  ;;
  ;; - The reserved keyword `this`. Marked with the current class
  ;;   type.
  [this
   (c-type+ #'*this)]
  ;; - A local binding (from a def or let). Marked with its binder.
  [ID:id #:when (is-locally-binded? #'ID)
         (binder+ #'ID)]
  ;; - A top level binding (no binder). In that case, it presumably
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
  ;;   ID ∗> *(get-field this ID)
  [ID:id
   #:with *ID (c-type+ #'ID)
   (∗> #'(get-field this *ID))]

  [_ (error "Unknown syntax" this-syntax)]))


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


(provide (rename-out
          [surface-read read]
          [surface-read-syntax read-syntax]))

;; Reader
(define (surface-read in)
  (syntax->datum (surface-read-syntax #f in)))


(define (surface-read-syntax src in)
  (define the-s-exps
    (let s-exps ([s-exp (read-syntax src in)])
      (if (eof-object? s-exp)
          '()
          (cons s-exp (s-exps (read-syntax src in))))))

  (define prog-sexp (quasisyntax/loc (car the-s-exps) (prog #,@the-s-exps)))
  (pretty-print (syntax->datum (∗> prog-sexp)))

    ;; (define the-s-exp #`(#,@(s-exps)))
    ;; (d* #`(prog #,@the-s-exp)))

  ;; ;;   (match (read-syntax src in)
  ;; ;;     [eof '()]
  ;; ;;     [_ (cons sexp (desugar))]
  ;; ;;   ))
  ;; ;; (letrec ([desugar (λ () (read-syntax src in))])
  ;; ;;   ()
  ;; ;;   )

  ;; ;; TODO: use `read-syntax' of s-exp then parten match on it
  ;; (define prog (desugar))
  ;; (displayln prog)

  ;; #`(module m "surface-desugar-lang.rkt" #,@prog)
  ;; (error "lala")
  #'(module m racket/base (void))

  )

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
