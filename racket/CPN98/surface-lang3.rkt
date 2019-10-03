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

(require
 (for-meta 2 racket/base
             syntax/parse
             syntax/transformer
             syntax/parse/define
             racket/syntax
             racket/match
             racket/list
             racket/stxparam
             "type-checker.rkt"
             "utils.rkt"
             )
 (for-syntax racket/base
                     syntax/parse
                     syntax/parse/define
                     syntax/transformer
                     syntax/free-vars
                     racket/syntax
                     racket/match
                     racket/list
                     racket/stxparam
                     "type-checker.rkt"
                     "utils.rkt"
                     )
         syntax/parse
         syntax/parse/define
         racket/match
         racket/stxparam
         ;;
         "type-checker.rkt"
         )

;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects





;; Utils

(begin-for-syntax

  ;; -- Syntax checker in the form of
  ;; https://docs.racket-lang.org/syntax/syntax-helpers.html#%28part._stxkeyword%29
  ;; A check procedure consumes the syntax to check and a context
  ;; syntax object for error reporting and either raises an error to
  ;; reject the syntax or returns a value as its parsed
  ;; representation.

  ;; Returns the first duplicate class in the program or #f if there
  ;; are no duplicate.
  (define (check-class clss-stx)
    (define (get-class-name cls-stx)
      (syntax-case cls-stx (class)
        [(class name)          #'name]
        [(class name _ ...) #'name]
        [_ (raise-syntax-error #f "Bad syntax for class" cls-stx)]))

    (let* ([classes     (syntax->list clss-stx)]
           [class-names (map get-class-name classes)])
      (check-duplicate-identifier class-names)))

  ;; Returns the first duplicate field in the class or #f if there are
  ;; no duplicate.
  (define (field-twice? cls-stx)
    (define (get-field-name field/def-stx)
      (syntax-case field/def-stx (field :)
        ;; (field [NAME : TYPE])
        [(field [name : _]) #'name]
        [_                  #f]))

    (let* ([fields/defs (syntax->list cls-stx)]
           [field-names (filter-map get-field-name fields/defs)])
      (check-duplicate-identifier field-names)))

  ;; Returns the first duplicate def in the class or #f if there are
  ;; no duplicate.
  (define (def-twice? cls-stx)
    (define (get-def-name field/def-stx)
      (syntax-case field/def-stx (def : →)
        ;; (def (NAME [ARG:NAME : ARG:TYPE] ... → RET:TYPE) E)
        [(def (name _ ...) _) #'name]
        [_                      #f]))

    (let* ([fields/defs (syntax->list cls-stx)]
           [def-names   (filter-map get-def-name fields/defs)])
      (check-duplicate-identifier def-names)))

  (define (forall p l)
    (for/and ([i l]) (p i)))

  (define-literal-set keyword-lits
    ;; Note: I have to define new, send, ... as datum otherwise they
    ;; are going to be interpreted as identifier during macro
    ;; expansion and may risk an evaluation with new, send from
    ;; racket/class.
    #:datum-literals (new send get-field set-field! this)
    ;; I have no literals that should be interpreted.
    ())

  ;; Check `#%app F ARG ...` is one of defined keywords
  (define is-keyword? (literal-set->predicate keyword-lits))

  ;; (define (is-expr? E)
  ;;   ;; Check `E` is an expression
  ;;   (define expr (syntax-e E))
  ;;   (cond
  ;;     [(symbol? expr) #t]  ;; variable
  ;;     [(number? expr) #t]
  ;;     [else
  ;;      (let ([expr (syntax-e (car expr))])
  ;;        (member expr '(new let send get-field set-field!)))]))

  ;; -- Syntax class for type and arg
  (define-syntax-class type
    #:description "class' type with ownership and context parameters"
    #:attributes [TYPE OWNER CPARAMS]
    #:datum-literals (/)  ;; Don't consider '/' as a pattern
    (pattern (O:id / T:id)
             #:with OWNER #'O
             #:with TYPE #'T
             #:with CPARAMS #''())
    (pattern (O:id / (T:id PARAMS:id ...+))
             #:with OWNER #'O
             #:with TYPE #'T
             #:with CPARAMS #''(PARAMS ...))
    (pattern T:id
             #:with OWNER #''Θ
             #:with TYPE #'T
             #:with CPARAMS #''())
    (pattern (T:id PARAMS:id ...+)
             #:with OWNER #''Θ
             #:with TYPE #'T
             #:with CPARAMS #''(PARAMS ...))
    )

  (define-syntax-class arg
    #:description "argument with its type"
    #:datum-literals (:)
    (pattern (NAME:id : T:type)
             #:attr OWNER #'T.OWNER
             #:attr TYPE  #'T.TYPE
             #:attr CPARAMS #'T.CPARAMS))

  ;; Format TYPE
  ;; (define (fTYPE TYPE)
  ;;   (syntax-parse TYPE
  ;;     [(t:type) #'(t.TYPE t.OWNER t.CPARAMS)]
  ;;     [(a:arg) #'(a.TYPE a.OWNER a.CPARAMS)]))

  )

;; Syntax parameters, as racket `parameter`, but at transformer level
;; - https://beautifulracket.com/explainer/parameters.html#syntax-parameters
;; - https://www.greghendershott.com/fear-of-macros/Syntax_parameters.html
;;
;; These parameters can be accessed inside a syntax object and give
;; the binded stx object in return. E.g., in the following,
;; `#'my-param` syntax object is, actually, a binder for the `#'42`
;; syntax object.
;;
;; > (define-syntax-parameter my-param (λ (stx) #'42))
;; > ...
;; > #'(writeln my-param) ; 42
;;
;; And I can later rebind it to another value with
;; `syntax-parameterize`, analogously to `parameterize` .
;;
;; These parameters should take place in a syntax-object for phase 0
;; (runtime). However, in the following, I use these parameters to
;; track information during phase 1 (macro expansion). Thus, I bind
;; them to a syntax error if someone try to use it a runtime.
(define-syntax-parameter current-class-type  ;; Syntax object that is
                                             ;; the type of the
                                             ;; current class.
  (λ (stx) (raise-syntax-error #f "Should only be used during macro expansion" stx)))

(define-syntax-parameter field-types        ;; Map of `(class-τ . field-name) -> field-τ`
  (λ (stx) (raise-syntax-error #f "Should only be used during macro expansion" stx)))

;;  Accessing it at phase 1 needs resort to `syntax-parameter-value`
(define-for-syntax (class-τ) (syntax-parameter-value #'current-class-type))
(define-for-syntax (check-∈f class-τ field-name [context-stx #f])
  (let ([fields-τ (syntax-parameter-value #'field-types)]
        [field-key (cons class-τ field-name)])
    (unless (hash-has-key? fields-τ field-key)
      (raise-syntax-error #f "Field not it class" context-stx field-name))
    (hash-ref fields-τ field-key)))

(define-for-syntax (field-τ class-τ field-name)
  (let ([fields-τ (syntax-parameter-value #'field-types)])
    (hash-ref fields-τ (cons class-τ field-name))))

(define-for-syntax (stx=? a b)
  (cond
    [(and (identifier? a) (identifier? b))
     (bound-identifier=? a b)]
    [(and (syntax? a) (syntax? b))
     (and (bound-identifier=? (datum->syntax a '||) (datum->syntax b '||))
          (stx=? (syntax-e a) (syntax-e b)))]
    [else
     (equal?/recur a b stx=?)]))

(define-for-syntax (add-τ e τ) (syntax-property e 'type τ))
(define-for-syntax (get-τ e) (syntax-property e 'type))
(define-for-syntax (compute-τ e) (get-τ (local-expand e 'expression null)))
(define-for-syntax (erase-τ e) (local-expand e 'expression null))
(define-for-syntax (comp+erase-τ e)
  (let* ([e- (local-expand e 'expression null)]
         [τ (get-τ e-)])
    (values e- τ)))
(define-for-syntax (check-τ τ1 τ2) (stx=? τ1 τ2))

;; Expander

;; ----- Class
;;  CLASS := (class NAME (C-PARAMS ...) FIELD ... DEF ...)
;;  ~> (class O-SCHEME NAME FIELD ... DEF ...)
(define-syntax-parser class
  ;; The `~!` eliminate backtracking. Hence, if the next `fail-when`
  ;; failed, it will not backtrack to the second pattern, which would
  ;; create an infinite loop.
  [(_ NAME:id [CPARAM:id ...] ~! FIELD/DEF:expr ...)
   #:fail-when (field-twice? #'(FIELD/DEF ...)) "Duplicated field"
   #:fail-when (def-twice? #'(FIELD/DEF ...)) "Duplicated def"
   #'(syntax-parameterize ([current-class-type #'NAME]
                           [field-types (hash)])
       `(#%class NAME [CPARAM ...] ,FIELD/DEF ...))]
     ;; #'`(#%class NAME [CPARAM ...] B)]
  [(_ NAME:id FIELD/DEF:expr ...)
   #'(class NAME [] FIELD/DEF ...)] )

;; ;;  FIELD := (field [NAME : TYPE])
;; ;;  ~> (field O-SCHEME NAME)
(define-syntax-parser field
  [(_ ARG:arg)
   #:with F-ARG (syntax->datum #'ARG)
   #'`(#%field F-ARG)])

;; DEF := (def (NAME [ARG:NAME : ARG:TYPE] ... → RET:TYPE) E)
;; ~> (def RET:O-SCHEME NAME (ARG:O-SCHEME ...) E)
(define-syntax-parser def
  #:datum-literals (→)
  [(_ (DNAME ARG:arg ... → RET:type)  BODY:expr)
   #:with D-ARGs (syntax->datum #'(DNAME ARG ... → RET))
   #'`(#%def D-ARGs ,BODY)])


;; (define-for-syntax (add-τ e τ) (syntax-property e 'type τ))
;; (define-for-syntax (get-τ e τ) (syntax-property e 'type))
;; (define-for-syntax (compute-τ e) (get-τ (local-expand e 'expression null)))
;; (define-for-syntax (check-τ τ1 τ2) (stx=? τ1 τ2))
;; (define-for-syntax (erase-τ e) (local-expand e 'expression null))
;; (define-for-syntax (comp+erase-τ e)
;;   (let* ([e- (local-expand e 'expression null)]
;;          [τ (get-τ e-)])
;;     (values e- τ)))
(define-syntax-parser @%app
  #:literal-sets ([keyword-lits])
  [(_ new CLASS:type)
   (add-τ #'`(#%new CLASS) #'CLASS.TYPE)]
  [(_ get-field E:expr FNAME:id)
   ;; #:with Cτ (compute-τ #'E)
   #:with Cτ #''Main ;; (compute-τ #'E)
   ;; #:when (unless (∈f #'Cτ #'FNAME)
   ;;          (raise-syntax-error #f
   ;;                              (format "Class ~s doesn't contains field ~s" #'Cτ #'FNAME)
   ;;                              this-syntax))

   ;; (printf "~s:~s~n" this-syntax #'Cτ)
   ;; #:with Fτ ;; TODO: look into a store such as A for (Cτ . FNAME)
   ;;           ;; then add-τ Fτ
   #'`(#%get-field ,E FNAME)]
  [(_ set-field! E1:expr FNAME:id E2:expr)
   #'`(#%set-field! ,E1 FNAME ,E2)]
  [(_ send E:expr DNAME:id ARG:expr ...)
   #'`(#%send ,E DNAME ,ARG ...)]
  )

;; `(@%top . id)` refers to a top-level variable. In my lang, every
;; var is considered as a top-level variable since no racket `let`
;; binds it. There is two kind of var:
(define-syntax-parser @%top
  #:literal-sets ([keyword-lits])

  ;; - The var is `this`, e.g., `(get-field (@%top . this) my-field)`
  ;;   -#% I rebind it to the core #%this.
  [(_ . this)
   (add-τ #''#%this (class-τ))]

  ;; - The var is presumably a local field of the current class, that
  ;;   is, a sort of shortcut for (get-field this var) -- `var` instead of
  ;;   `this.var` in terms of Java. E.g.,
  ;;   > (class C
  ;;   >   (field [var : D])
  ;;   >   (def (get-f → D) (@%top . var)))
  ;;   -#% I expand it to the normal form with `this`:
  ;;       > (get-field (@%top . this) var)
  [(_ . ID:id)
   #'(@%app get-field (@%top . this) ID)])

(define-syntax-parser @let
  #:datum-literals (:)
  [(_ ((ANAME:id : ATYPE:type E:expr) BINDING ...) BODY:expr)
    ;; A `@let` with multiple binding into multiple successive `@let`s
    ;; with one unique binding.
   #:with B1 #'(ANAME : ATYPE E)  ;; The first binding of `@let`
   #:with BS #'(BINDING ...)      ;; Remaining bindings of `@let`
   ;; #:with bind-name (syntax-local-lift-expression #'ANAME)
   (syntax-case #'BS ()
     ;; FIXME: my ANAME should bind ANAME in BODY so that every (@%top
     ;; . var) if BODY refers to a top variable.
     ;; (define-syntax my-or (make-rename-transformer #'ANAME))
     [() #'`(#%let (ANAME : ATYPE ,E) ,BODY)]
     [_  #'(@let (B1) (@let BS BODY))])])

(define-syntax-parser @%module-begin
  [(_ CLASS:expr ... E:expr)
   #:fail-when (check-class #'(CLASS ...)) "Duplicated class name"
   ;; Check `CLASS ...` is a list of class definition
   ;; #:when (forall is-class? (syntax->list #'(CLASS ...)))
   ;; #:with PROG (syntax-local-expand-expression #'(CLASS ... E)
   ;;                                             #f
                             ;; )
   #'(#%module-begin
       (writeln `(#%prog ,CLASS ... ,E)))

   ;; (syntax-case #'PROG ()
   ;;   [(module nm lng (#%plain-module-begin . body))
   ;;    #`(#%plain-module-begin
   ;;       (#%require lng)
   ;;       . #,((make-syntax-introducer) #'body))
   ;;    ])
   ;; #'(#%module-begin
   ;;    (module surface-module "surface-desugar-lang.rkt"
   ;;      prog))
   ])

(provide (rename-out
          [@%module-begin #%module-begin]
          [@%top #%top] ;; wrapped unbound variables
          [@let let]
          [@%app #%app]
          )
         class field def
         ownership-scheme
         ;; current-class
         #%top-interaction
         )

;; ;; ---- Value
;; ;; (define-syntax-parser @%datum
;; ;;   [(_ . N:nat) #''(Num  N)])
