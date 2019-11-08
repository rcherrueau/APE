#lang racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Basic checking transformation (?>)
;;
;; - Checks no duplicate class/field/def names.
;; - Type checks the program (for simple type -- "simple" as in simply
;;   typed λ calculus, i.e., no ownership).
;; - Based on [FKF98] (see Bibliography)
;;
;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects
;;
;; Environments:
;; - Γ is the map (var . type) of locally bounded variables
;; - τ is the type of the current class

(require (for-syntax racket/base)
         racket/function
         racket/list
         racket/match
         syntax/parse
         syntax/parse/define
         syntax/stx
         "utils.rkt"
         "definitions.rkt")

(provide ?>)


;; ?> :: stx -> stx
(define-parser ?>
  #:literal-sets [*expr-lits]

  ;; ⊢p P ⇒ ?P : t
  ;;
  ;; `P` elaborates to `?P` with type `t`
  ;;
  ;; [prog]
  [(prog ~! CLASS ... E)
   ;; Check no duplicated class names; store them in CS
   #:fail-when (class-twice? #'(CLASS ...)) "Duplicated class name in prog"
   #:and (~do (stx-map CS-set! #'(CLASS ...)))
   ;; Elaborate
   ;;   P ⊢d CLASS ⇒ ?CLASS
   #:with [?CLASS ...] (stx-map ?> #'(CLASS ...))
   ;;   P,[] ⊢e E ⇒ ?E : t
   #:with ?E           (with-Γ #hash() (?> #'E))
   #:with t (dbg (type-prop #'?E))
   ;; ------------------------------------------------------------------
   ;;   ⊢p prog CLASS ... E ⇒ prog ?CLASS ... ?E : t
   (type-prop this-syntax #'t)]


  ;; P ⊢d CLASS ⇒ ?CLASS
  ;;
  ;; `CLASS` elaborates to `?CLASS` (under `P`)
  ;;
  ;; [defn]
  [(class ~! NAME [CPARAM ...] FIELD/DEF ...)
   #:with [FIELD ...] (filter field? (stx->list #'(FIELD/DEF ...)))
   #:with [DEF ...] (filter def? (stx->list #'(FIELD/DEF ...)))
   ;; Check no duplicated field names; store them in FS
   #:fail-when (field-twice? #'(FIELD ...)) "Duplicated field name in class"
   #:and (~do (stx-map (curry FS-set! #'NAME) #'(FIELD ...)))
   ;; Check no duplicated def names; store them in DS
   #:fail-when (def-twice? #'(DEF ...))     "Duplicated def name in class"
   #:and (~do (stx-map (curry DS-set! #'NAME) #'(DEF ...)))
   ;; Elaboration
   ;;   P ⊢τ t
   #:with [(field ~! F-NAME F-OW-SCHEME:ow-scheme) ...] #'(FIELD ...)
   #:when (stx-map ?> #'(F-OW-SCHEME ...))
   ;;   P,NAME ⊢m DEF ⇒ ?DEF
   #:with [?DEF ...] (parameterize ([τ #'NAME]) (stx-map ?> #'(DEF ...)))
   ;; -------------------------------------------------------------------
   ;;   P ⊢d class FIELD ... DEF ... ⇒ class FIELD ... ?DEF ...
   this-syntax]


  ;; P,τ ⊢m DEF ⇒ ?DEF
  ;;
  ;; `DEF` in `τ` elaborates to `?DEF`
  ;;
  ;; [meth]
  [(def ~! (NAME (ARG-NAME ARG-OW-SCHEME) ... RET-OW-SCHEME:ow-scheme) E)
   #:with τ0 (τ)
   ;; Elaborate:
   ;;   P ⊢τ t
   #:when (?> #'RET-OW-SCHEME)
   ;;   P ⊢τ t
   #:when (stx-map ?> #'(ARG-OW-SCHEME ...))
   ;;   P,{this: τ0, ARG-NAME: ARG-OW-SCHEME.TYPE, ...} ⊢e E ⇒ ?E : RET-OW-SCHEME
   #:with ?E
   ;; (let* ([this (cons #'this #'τ0)]
   ;;        [args (stx-map (syntax-parser
   ;;                         [(VAR OWS:ow-scheme) (cons #'VAR #'OWS.TYPE)])
   ;;                       #'([ARG-NAME ARG-OW-SCHEME] ...))])
   ;;   (with-Γ (cons this args)
   ;;     (?> #'E)))
   ;; TODO:
   (with-Γ (dbg #'((#'this #'τ0) (#'ARG-NAME #'ARG-OW-SCHE) ...))
     (?> #'E))
   ;;
   ;; TODO:
   ;; #:with ?t (check-⊢e #'?E #'RET-OW-SCHEME.TYPE)
   ;; ------------------------------------------------------------------
   ;;   P,τ0 ⊢m (ARG-NAME ARG-OW-SCHEME) ... RET-OW-SCHEME E ⇒
   ;;             (ARG-NAME ARG-OW-SCHEME) ... RET-OW-SCHEME ?E
   this-syntax]


  ;; P,Γ ⊢e E ⇒ ?E : t
  ;;
  ;; `E` elaborates to `?E` with type `t`
  ;;
  ;; [let]
  [(let ~! (VAR-NAME VAR-OW-SCHEME:ow-scheme E) BODY)
   ;; Elaborate:
   ;;   P ⊢τ VAR-OW-SCHEME
   #:when (?> #'VAR-OW-SCHEME)
   ;;   P,Γ ⊢e E => ?E : VAR-OW-SCHEME
   #:with ?E (?> #'E)
   ;; TODO:
   ;; #:when (check-⊢e #'?E #'VAR-OW-SCHEME.TYPE)
   ;;   P,Γ{VAR-NAME: VAR-OW-SCHEME} ⊢e BODY => ?BODY : t
   #:with t (type-prop
    (with-Γ (Γ-set #'VAR-NAME #'VAR-OW-SCHEME.TYPE)
      (?> #'BODY)))
   ;; ------------------------------------------------------------------
   ;;   P,Γ ⊢e *let (VAR-NAME VAR-OW-SCHEME E) BODY ⇒
   ;;             *let (VAR-NAME VAR-OW-SCHEME ?E) ?BODY : t
   (type-prop this-syntax #'t)]

  ;; [new]
  [(new ~! OW-SCHEME:ow-scheme)
   ;; Elaborate
   ;;   P ⊢τ VAR-OW-SCHEME
   #:when (?> #'OW-SCHEME)
   (type-prop this-syntax #'OW-SCHEME.TYPE)]

  ;; [get]
  [(get-field ~! E FNAME)
   ;; Elaborate
   ;;   P,Γ ⊢e E ⇒ ?E : ?t
   #:with ?E (?> #'E)
   ;; TODO:
   ;; #:fail-when (∉p #'?t #'FNAME) "Field does not exist in class"
   ;; Tag `this-syntax' with type of FNAME
   #:with t:ow-scheme (FS-type #'?E #'FNAME
                                #:context? this-syntax)
   (type-prop this-syntax #'t.TYPE)]

  ;; [set]
  [(set-field! ~! E FNAME BODY)
   ;; Elaborate
   ;;   P,Γ ⊢e E ⇒ ?E : ?t
   #:with ?t (type-prop (?> #'E))
   #:when (?> #'BODY)
   this-syntax]

  ;; [call]
  [(send ~! E DNAME PARAM ...)
   ;; Elaborate:
   ;;   P,Γ ⊢e E ⇒ ?E : ?t
   #:with ?t (type-prop (?> #'E))
   ;;   P ⊢τ PARAM ...
   #:when (stx-map ?> #'(PARAM ...))
   ;;   TODO: DS-type should include PARAM
   #:with t:ow-scheme (DS-type #'?t #'DNAME)
   (type-prop this-syntax (dbg #'t.TYPE))]

  ;; [var]
  [ID:id #:when (Γ? #'ID)
   #:with t (Γ-ref #'ID)
   (type-prop this-syntax #'t)]
  [ID:id ;; Not locally binded? ⇒ unbound identifier
   (raise-syntax-error #f "unbound identifier :(" #'ID)]

  ;; P ⊢τ t
  ;;
  ;; `t` exists (in `P`)
  ;;
  ;; [type]
  [OWS:ow-scheme
   #:fail-when (check-⊢τ #'OWS.TYPE) "Unexpected Type"
   this-syntax])


;; Environment

;; ~~~~~~~~~~~~~~~~~~~~
;; Manage local binding
;; ~~~~~~~~~~~~~~~~~~~~

(define τ (make-parameter #f))

;; Map of local bindings
;; : -> (Hash (Var . TYPE))
(define Γ (make-parameter #hash()))

;; Is VAR bounded?
;; : VAR -> Boolean
(define (Γ? VAR)
  (hash-has-key? (Γ) (syntax->datum VAR)))

;; Set TYPE of VAR in Γ
;; : VAR -> Boolean
(define (Γ-set VAR TYPE)
  (hash-set (Γ) (syntax->datum VAR) TYPE))

;; Returns the TYPE of VAR.
(define (Γ-ref VAR)
  (hash-ref (Γ) (syntax->datum VAR)))


;; Make `the-Γ` a new value for (Γ) parameter by mapping it into a
;; (Hash (Var . TYPE)) in the context of STX.
;; : (U (Hash (Var . TYPE)) (List (VAR . TYPE)) (-> STX) -> Void
(define (private:with-Γ the-Γ thunk-E)
  #;(define listof-datum? (listof symbol?))

  (parameterize
      ([Γ (cond
            [(hash? the-Γ) the-Γ]
            [(and (syntax? the-Γ) (stx->list the-Γ))
             => (curry map syntax->datum)]
            [(syntax? the-Γ) (list (syntax->datum the-Γ))]
            #;[(listof-datum? the-Γ) the-Γ]
            [(symbol? the-Γ) (list the-Γ)]
            [else (raise-argument-error
                   'with-Γ
                   "(or/c syntax? (listof syntax?) (listof datum?))"
                   the-Γ)])])
    (dbg (Γ))
    (thunk-E)))


(define-syntax-parser with-Γ
  ;; make THE-Γ a new value for (Γ). THE-Γ could be e new hashmap or a
  ;; list of assoc to build the new hashmap from.
  [(_ THE-Γ E:expr)
   #'(let ([the-Γ THE-Γ]) ;; Reduce THE-Γ to compute it only once
       (parameterize
           ([Γ (if (hash? the-Γ)
                   ;; It's an HASH, proceed,
                   the-Γ
                   ;; It's a list of assoc: make all keys of the assocs
                   ;; a datum and then make the hash
                   (let*-values ([(keys values) (unzip the-Γ)]
                                 [(datums) (map syntax->datum keys)])
                     (make-immutable-hash (zip datums values))))])
         E))])


;; Utils

;; Returns `#t` if the syntax object is a field.
;; field? : Syntax -> Boolean
(define field?
  (syntax-parser
    #:literal-sets [*expr-lits]
    [(field _ ...) #t]
    [_ #f]))

;; Returns `#t` if the syntax object is a def.
;; def? : Syntax -> Boolean
(define def?
  (syntax-parser
    #:literal-sets [*expr-lits]
    [(def _ ...) #t]
    [_ #f]))

;; -- Syntax checker in the form of
;; https://docs.racket-lang.org/syntax/syntax-helpers.html#%28part._stxkeyword%29
;; A check procedure consumes the syntax to check and a context
;; syntax object for error reporting and either raises an error to
;; reject the syntax or returns a value as its parsed
;; representation.
;;
;; Returns the first duplicate class in the program or #f if there
;; are no duplicate.
(define (class-twice? clss-stx)
  (define (get-class-name cls-stx)
    (syntax-parse cls-stx
      #:literal-sets [*expr-lits]
      [(class name _ ...) #'name]))

  (let* ([classes     (syntax->list clss-stx)]
         [class-names (map get-class-name classes)])
    (check-duplicate-identifier class-names)))

;; Returns the first duplicate field in the class or #f if there are
;; no duplicate.
(define (field-twice? field-stxs)
  (define get-field-name
    (syntax-parser
      #:literal-sets [*expr-lits]
      [(field name _ ...) #'name]))

  (define field-names (map get-field-name (stx->list field-stxs)))
  (check-duplicate-identifier field-names))

;; Returns the first duplicate def in the class or #f if there are
;; no duplicate.
(define (def-twice? def-stxs)
  (define get-def-name
    (syntax-parser
      #:literal-sets [*expr-lits]
      [(def (name _ ...) _) #'name]))

    (define def-names (map get-def-name (stx->list def-stxs)))
    (check-duplicate-identifier def-names))

;; Returns #f if TYPE is expected.
(define (check-⊢τ TYPE)
  (if (CS-member TYPE) #f TYPE))

;; Returns (type-prop E) if (type-prop E) has type TYPE, raise a
;; syntax error otherwise
(define (check-⊢e E TYPE)
  (define expected-type (syntax->datum TYPE))
  (define e-type (syntax->datum (type-prop E)))

  (cond
    [(eq? expected-type e-type) (type-prop E)]
    [else
     (define error-msg "type mismatch;~n  expected: ~s~n  given: ~s")
     (raise-syntax-error #f (format error-msg expected-type e-type) E)]))

;; Extracts from an OW-SCHEME a SIMPLE-TYPE
(define ow-type->simple-type (syntax-parser [(_ t _) #'t]))


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
