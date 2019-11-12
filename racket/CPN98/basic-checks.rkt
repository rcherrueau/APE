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
;; - Based on [FKF98] (see Bibliography).
;;
;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects
;;
;; Environments:
;; - Γ is the map (var . type) of locally bounded variables
;; - τ is the type of the current class

(require (for-syntax racket/base)
         racket/contract/base
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
   #:with [?DEF ...] (with-τ #'NAME (stx-map ?> #'(DEF ...)))
   ;; -------------------------------------------------------------------
   ;;   P ⊢d class FIELD ... DEF ... ⇒ class FIELD ... ?DEF ...
   this-syntax]


  ;; P,τ ⊢m DEF ⇒ ?DEF
  ;;
  ;; `DEF` in `τ` elaborates to `?DEF`
  ;;
  ;; [meth]
  [(def ~! (NAME (ARG-NAME ARG-OW-SCHEME:ow-scheme) ... RET-OW-SCHEME:ow-scheme) E)
   #:with τ0 (τ)
   ;; Elaborate:
   ;;   P ⊢τ t
   #:when (stx-map ?> #'(ARG-OW-SCHEME ...))
   ;;   P ⊢τ t
   #:when (?> #'RET-OW-SCHEME)
   ;;   P,{this: τ0, ARG-NAME: ARG-OW-SCHEME.TYPE, ...} ⊢e E ⇒ ?E : RET-OW-SCHEME
   #:with ?E (with-Γ #'((this . τ0) (ARG-NAME . ARG-OW-SCHEME.TYPE) ...)
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
    (with-Γ (Γ-set #'(VAR-NAME . VAR-OW-SCHEME.TYPE))
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
  [ID:id ;; Not locally binded? ⇒ unbound identifier. This is not
         ;; supposed to happened thanks to desugaring.
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

;; Store the current class type
(define τ (make-parameter #f))

;; Make `the-τ` a new value for (τ) parameter in the context of STX.
;; : TYPE (-> STX) -> STX
(define (private:with-τ the-τ thunk-E)
  (parameterize ([τ the-τ]) (thunk-E)))

(define-syntax-parser with-τ
  ;; Automatically create the `thunk` around E expression
  [(_ THE-τ E:expr) #'(private:with-τ THE-τ (thunk E))])

;; Map of local bindings
;; : -> (Hash (Var . TYPE))
(define Γ (make-parameter #hash()))

;; Is VAR bounded?
;; : VAR -> Boolean
(define (Γ? VAR)
  (hash-has-key? (Γ) (syntax->datum VAR)))

;; Set TYPE of VAR in Γ
;; : (VAR . TYPE) -> Boolean
(define (Γ-set VAR.TYPE)
  (define VAR (car (syntax-e VAR.TYPE)))
  (define TYPE (cdr (syntax-e VAR.TYPE)))
  (hash-set (Γ) (syntax->datum VAR) TYPE))

;; Returns the TYPE of VAR.
(define (Γ-ref VAR)
  (hash-ref (Γ) (syntax->datum VAR)))

;; Make `the-Γ` a new value for (Γ) parameter by mapping it into a
;; (Hash (Var . TYPE)) in the context of STX.
;; : (U (Hash (Var . TYPE)) ((VAR . TYPE) ...) (-> STX) -> STX
(define (private:with-Γ the-Γ thunk-E)
  (define listof-datum? (listof symbol?))
  (define listof-stx? (listof syntax?))
  (define (hashof-Var-TYPE? hs)
    (and (hash? hs)                     ;; This is an hash, and
         (or (hash-empty? hs)           ;; - Whether it is empty
             (let ([ks (hash-keys hs)]  ;; - Or it hash (Var . TYPE) elems
                   [vs (hash-values hs)])
               (and (listof-datum? ks)
                    (listof-stx? vs))))))

  (define (VAR-TYPE...? xs)
    ;; This is syntax object
    (and (syntax? xs)
         ;; That reduces to a list of pair of syntax objects
         ;; ((VAR TYPE) ...)
         (let ([XS  (syntax->list xs)])
           (if XS
               (let ([KVs (map syntax-e XS)])
                 (for/and ([kv KVs])
                   (if (and (pair? kv)
                            (syntax? (car kv))
                            (syntax? (cdr kv)))
                       KVs #f)))
               #f)
           )))


  (parameterize
      ([Γ (cond
            [(hashof-Var-TYPE? the-Γ) the-Γ]
            [(VAR-TYPE...? the-Γ)
             => (λ (the-Γ)
                  (let-values ([(keys values) (unzip the-Γ)])
                    (make-immutable-hash (zip (map syntax->datum keys) values))))]
            [else (raise-argument-error
                   'with-Γ
                   "(or/c syntax? (hash? datum? syntax?))"
                   the-Γ)])])
    (dbg (Γ))
    (thunk-E)))

(define-syntax-parser with-Γ
  ;; Automatically create the `thunk` around E expression
  [(_ THE-Γ E:expr) #'(private:with-Γ THE-Γ (thunk E))])


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


;; Tests
(module+ test
  (require rackunit)

  (test-exn "Duplicated class name"
            #rx"Duplicated class name"
            (thunk (?> #'(prog (class X []) (class X []) _))))

  (test-exn "Duplicated field name"
            #rx"Duplicated field name"
            (thunk (?> #'(class X [] (field x _) (field x _)))))

  (test-exn "Duplicated def name"
            #rx"Duplicated def name"
            (thunk (?> #'(class X [] (def (x _) _) (def (x _) _)))))

  ;; No unbound identifier everywhere an `E` is expected. Desugaring
  ;; ensures that all identifiers are binded with a let
  (test-begin
    (test-exn "Unbound identifier"
              #rx"unbound identifier.+?in: id"
              (thunk (?> #'id)))

    (test-exn "Unbound identifier in prog"
              #rx"unbound identifier.+?in: id"
              (thunk (?> #'(prog (class X []) id))))

    ;; FIXME:
    ;; (test-exn "Unbound identifier in class"
    ;;           #rx"unbound identifier.+?in: id"
    ;;           (thunk (?> #'(class X []  id))))

    )

  (test-begin
    (test-exn "Unknown type in field"
              #rx"Unexpected Type.+?at: UnknownType"
              (thunk (?> #'(class X [] (field x (ow-scheme UnknownType _ ()))))))

    (test-exn "Unknown return type in def"
              #rx"Unexpected Type.+?at: UnknownType"
              (thunk (?> #'(def (x (ow-scheme UnknownType _ ())) _))))

    (test-exn "Unknown arg type in def"
              #rx"Unexpected Type.+?at: UnknownType"
              (thunk (?> #'(def (x (_ (ow-scheme UnknownType _ ()))
                                   (ow-scheme _ _ ())) _))))

    (test-exn "Unknown type in let"
              #rx"Unexpected Type.+?at: UnknownType"
              (thunk (?> #'(let (x (ow-scheme UnknownType _ ()) _) _))))

    (test-exn "Unknown type new"
              #rx"Unexpected Type.+?at: UnknownType"
              (thunk (?> #'(new (ow-scheme UnknownType _ ())))))
      )

  )
