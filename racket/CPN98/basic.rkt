#lang racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.
;;
;; Basic checking transformation (?>)
;; - Checks no duplicate class/field/def names.
;; - Type checks the program (for simple type -- "simple" as in simply
;;   typed λ calculus, i.e., no ownership).
;; - Based on [FKF98] (see Bibliography).
;;
;; Environments:
;; - Γ is the map of locally bound variables
;; - τ is the type of the current class
;; - CS is the set of defined types
;; - FS is the map of fields
;; - DS is the map of definitions
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
         racket/contract/base
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
         (rename-in "definitions.rkt"
                    [dict-ref      def/dict-ref]
                    [dict-has-key? def/dict-has-key?]
                    [dict-map      def/dict-map]))

(provide ?>)


;; Transformation (?>)

(define-parser (?> stx)
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #:Env
  (;; Map of locally bound variables.
   ;; Γ : (HashTable Identifier B-TYPE)
   [Γ (make-immutable-id-hash)]

   ;; Store of the current class type
   ;; τ : B-TYPE
   [τ #'Unit]

   ;; Set of types
   ;; CS : (Listof B-TYPE)
   [CS (unbox meta:CS)]

   ;; Map of fields
   ;;
   ;; With the syntax #'(Class-type . Field-name) as key and the Field
   ;; type as value.
   ;;
   ;; FS : (Dict (Syntaxof (Pairof (B-TYPE Identifier))) B-TYPE))
   [FS (def/dict-map
         (syntax-parser [SCHEME:ow-scheme #'SCHEME.TYPE])
         (unbox meta:FS))]

   ;; Map of definitions
   ;;
   ;; With the syntax #'(Class-type Def-name (Def-arg-type ...)) as
   ;; key and the Def return type as value.
   ;;
   ;; DS : (Dict (Syntaxof (B-TYPE Identifier (Listof B-TYPE))) B-TYPE))
   [DS (def/dict-map
         (syntax-parser [SCHEME:ow-scheme #'SCHEME.TYPE])
         (unbox meta:DS))])

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Parse
  (⊢p stx))


;; Rules

(define-syntax define-rules
  (syntax-parser
    [(_ ID:id RULE:expr ...)
     #'(define ID
         (λ (stx)
           ;; (dbg stx #:ctx ID)
           (syntax-parse stx
             #:literal-sets [(keyword-lits #:at ID) (expr-lits #:at ID)]
             RULE ...
             )))]))

;; ⊢p P ≫ ?P : t
;;
;; `P` elaborates to `?P` and has type `t`
(define-rules ⊢p
  ;; [prog]
  [(prog ~! CLASS ... E)
   ;; Check no duplicated class names
   #:when (no-name-clash? #'(CLASS ...))
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
   ;; Check no duplicated field and def names
   #:when (no-name-clash? #'(FIELD ...))
   #:when (no-name-clash? #'(DEF ...))
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
   #:when (τ=? #'t-e #'RET.TYPE #:srcloc #'?E)
   ;; ----------------------------------------------------------------
   ;; P,τ0 ⊢m (def (NAME (ARG-NAME ARG-OW-SCHEME) ... RET-OW-SCHEME) E) ≫
   ;;           (def (NAME (ARG-NAME ARG-OW-SCHEME) ... RET-OW-SCHEME) ?E)
   this-syntax])


;; P,Γ ⊢e E ≫ ?E : t
;;
;; In context `P,Γ`, `E` elaborates to `?E` and has type `t`
(define-rules ⊢e
  ;; [new]
  [(new ~! SCHEME:ow-scheme)
   ;; Check P ⊢τ VAR-OW-SCHEME
   #:when (⊢τ #'SCHEME.TYPE)
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e (new C) ≫ (new C) : C
   (add-τ this-syntax #'SCHEME.TYPE)]

  ;; [var]
  ;; Check ID ∈ dom(Γ)
  ;;;; ID is locally bound (including `this` and `???`).
  [ID:id #:when (Γ-member? #'ID)
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e ID ≫ ID : Γ(ID)
   (add-τ this-syntax (Γ-ref #'ID))]
  ;;; Unbound identifier. This is not supposed to happened thanks to
  ;;; desugaring, but who knows ...
  [ID:id
   (raise-syntax-error #f "unbound identifier" #'ID)]

  ;; [get]
  [(get-field ~! E FNAME)
   ;; Check P,Γ ⊢e E ≫ ?E : ?t
   #:with [?E t] (get-τ (⊢e #'E))
   ;; Check (t . FNAME) ∈ dom(FS)
   ;;;; The field FNAME is defined in the class t
   #:fail-unless (FS-member? #'(t . FNAME))
   (format "~a is not a field of ~a"
           (syntax->datum #'FNAME)
           (syntax->datum #'t))
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e (get-field E FNAME) ≫
   ;;          (get-field (?E : t) FNAME) : FS(t . FNAME)
   (add-τ this-syntax (FS-ref #'(t . FNAME)))]

  ;; [set]
  [(set-field! ~! E FNAME BODY)
   ;; Check P,Γ ⊢e E ≫ ?E : ?t
   #:with [?E t] (get-τ (⊢e #'E))
   ;; Check (t . FNAME) ∈ dom(FS)
   ;;;; The field FNAME is defined in the class t
   #:fail-unless (FS-member? #'(t . FNAME))
   (format "~a is not a field of ~a"
           (syntax->datum #'FNAME)
           (syntax->datum #'t))
   ;; Check P,Γ ⊢e BODY ≫ ?BODY : FS(t . FNAME)
   ;;;; The BODY has to elaborate to something that fit into the
   ;;;; field.
   #:with [?BODY t-body] (get-τ (⊢e #'BODY))
   #:with t-field (FS-ref #'(t . FNAME))
   #:when (τ=? #'t-body #'t-field)
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e (set-field E FNAME BODY) ≫
   ;;          (set-field (?E : t) FNAME ?BODY) : FS(t . FNAME)
   (add-τ this-syntax #'t-field)]

  ;; [call]
  [(send ~! E DNAME PARAM ...)
   ;; Check P,Γ ⊢e E ≫ ?E : t
   #:with [?E t] (get-τ (⊢e #'E))
   ;; Check P,Γ ⊢e PARAM ... ≫ (?PARAM : t) ...
   #:with [(?PARAM t-param) ...] (stx-map (∘ get-τ ⊢e) #'(PARAM ...))
   ;; Check (t DNAME (t-param ...)) ∈ dom(DS)
   ;;;; The method DNAME with parameters (t-param ...) is defined in
   ;;;; the class t.
   #:with DS-key #'(t DNAME (t-param ...))
   #:fail-unless (DS-member? #'DS-key)
   (format "~a with arguments ~a is not a method of ~a"
           (syntax->datum #'DNAME)
           (syntax->datum #'(t-param ...))
           (syntax->datum #'t))
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e (send E DNAME PARAM ...) ≫
   ;;          (send (?E : t) DNAME ?PARAM ...) : DS(t DNAME PARAM ...)
   (add-τ this-syntax (DS-ref #'DS-key))]

  ;; [let]
  [(let ~! (VAR-NAME VAR-SCHEME:ow-scheme E) BODY)
   #:when (⊢τ #'VAR-SCHEME.TYPE)
   ;; Check  P,Γ ⊢e E ≫ ?E : VAR-OW-SCHEME
   #:with [?E t] (get-τ (with-Γ (Γ-set #'(??? . VAR-SCHEME.TYPE))
                          (⊢e #'E)))
   #:when (τ=? #'t #'VAR-SCHEME.TYPE #:srcloc #'?E)
   ;; Check P,Γ{VAR-NAME: VAR-OW-SCHEME} ⊢e BODY ≫ ?BODY : t
   #:with [_ t-body] (get-τ (with-Γ (Γ-set #'(VAR-NAME . VAR-SCHEME.TYPE))
                              (⊢e #'BODY)))
   ;; ------------------------------------------------------------------
   ;; P,Γ ⊢e *let (VAR-NAME VAR-OW-SCHEME E) BODY ≫
   ;;           *let (VAR-NAME VAR-OW-SCHEME ?E) ?BODY : t
   (add-τ this-syntax #'t-body)])

;; P ⊢τ t
;;
;; In context `P`, `t` exists
(define-rules ⊢τ
  ;; [type]
  [t #:when (CS-member? #'t) this-syntax]
  [t (raise-syntax-error #f "Unknown Type" #'t)])


;; Environment

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; Manage the current class type τ

;; Make `the-τ` a new value for (τ) parameter in the context of STX.
;; : TYPE (-> STX) -> STX
(define (private:with-τ the-τ thunk-E)
  (parameterize ([τ the-τ]) (thunk-E)))

(define-syntax-parser with-τ
  ;; Automatically create the `thunk` around E expression
  [(_ THE-τ E:expr ...) #'(private:with-τ THE-τ (thunk E ...))])

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; Manage local binding Γ

(define-custom-hash-types id-hash
  #:key? identifier?
  bound-id=?)

;; Is VAR bounded in Γ?
;; : VAR -> Boolean
(define (Γ-member? VAR)
  (dict-has-key? (Γ) VAR))

;; Set TYPE of VAR in Γ
;; > (Γ-set #'(VAR . TYPE))
;; : #'(VAR . TYPE) -> Boolean
(define (Γ-set VAR.TYPE [the-Γ #f])
  (let* ([the-Γ (if the-Γ the-Γ (Γ))]
         [VAR-&-TYPE (syntax-e VAR.TYPE)]
         [VAR (car VAR-&-TYPE)]
         [TYPE (cdr VAR-&-TYPE)])
    (dict-set the-Γ VAR TYPE)))

(define (Γ-set* VAR.TYPEs)
  (foldr Γ-set (Γ) (syntax->list VAR.TYPEs)))

;; Returns the TYPE of VAR in Γ
;; : VAR -> TYPE
(define (Γ-ref VAR)
  (dict-ref (Γ) VAR))

;; Make `the-Γ` a new value for Γ parameter by mapping it into a
;; (Hash (Var . TYPE)) in the context of STX.
;; : (U (Id-Hash (VAR . TYPE)) ((VAR . TYPE) ...) (-> STX) -> STX
(define (private:with-Γ the-Γ thunk-E)
  (parameterize
    ([Γ (cond
          [(immutable-id-hash? the-Γ) the-Γ]
          [(and (syntax? the-Γ) (stx->list the-Γ))
           => (∘ make-immutable-id-hash (curry map syntax-e))]
          [else (raise-argument-error
                 'with-Γ "(or/c syntax? immutable-id-hash?)" the-Γ)])])
    ;; (dbg (dict->list (Γ)))
    (thunk-E)))

(define-syntax-parser with-Γ
  ;; Automatically create the `thunk` around E expression
  [(_ THE-Γ E:expr ...) #'(private:with-Γ THE-Γ (thunk E ...))])

;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; Access set of types CS, fields FS, and defs DS info

;; Tests type `t` exists in CS.
(define (CS-member? t)
  (if (findf (curry bound-id=? t) (CS)) #t
      (raise-syntax-error #f "Unknown Type" t)))

;; Test two FS-key are equals.
;; (fs-key=? #'(a . b) #'(a . b))  ; #t
;; (fs-key=? #'(a . b) #'(c . d))  ; #f
;; (fs-key=? #'(a . 1) #'(a . 1))  ; #f
(define (fs-key=? key1-stx key2-stx)
  (match-let ([(cons C-TYPE1 F-NAME1) (syntax-e key1-stx)]
              [(cons C-TYPE2 F-NAME2) (syntax-e key2-stx)])
    (and
     (bound-id=? C-TYPE1 C-TYPE2)
     (bound-id=? F-NAME1 F-NAME2))))

;; (: FS-ref FS-key -> B-TYPE)
(define (FS-ref FS-key) (def/dict-ref (FS) FS-key fs-key=?))

;; (: FS-member FS-key -> Boolean)
(define (FS-member? FS-key) (def/dict-has-key? (FS) FS-key fs-key=?))


;; Test two DS-key are equals:
;; - Defined in the same class
;; - Same name
;; - Same argument types (type comparison does not look at the owner
;;   or context parameter).
;;
;; (: ds-key=? (DS-key DS-key -> Boolean))
(define (ds-key=? key1-stx key2-stx)
  (match-let ([(list C-TYPE1 D-NAME1 ARGs-OW-SCHEME1) (syntax-e key1-stx)]
              [(list C-TYPE2 D-NAME2 ARGs-OW-SCHEME2) (syntax-e key2-stx)])
    (and
     (bound-id=? C-TYPE1 C-TYPE2)
     (bound-id=? D-NAME1 D-NAME2)
     (for/and ([ow1 (syntax->list ARGs-OW-SCHEME1)]
               [ow2 (syntax->list ARGs-OW-SCHEME2)])
       ;; Compare two ow-scheme, but does not instantiate the owner and
       ;; context parameters. So it only ensures that both basic types are
       ;; bound and both contains the same number of context parameters.
       ;;
       ;; > (type=? #'(a b ()) #'(a b ()))            ; #t
       ;; > (type=? #'(a b [c d e]) #'(a b [c d e]))  ; #t
       ;; > (type=? #'(a b [c d e]) #'(a b [e d c]))  ; #t
       ;; > (type=? #'(a b [c d e]) #'(a z [c d e]))  ; #t
       ;; > (type=? #'(a b ()) #'(c b ()))            ; #f
       (type=? ow1 ow2)))))

;; (: DS-ref DS-key -> B-TYPE)
(define (DS-ref DS-key) (def/dict-ref (DS) DS-key ds-key=?))

;; (: DS-member DS-key -> Boolean)
(define (DS-member? DS-key) (def/dict-has-key? (DS) DS-key ds-key=?))


;; Exceptions

;; Name clash
(struct exn:name-clash exn:fail:syntax ()
  #:extra-constructor-name make-exn:name-clash
  #:transparent)

(define (raise-name-clash ID IDs)
  (define srcloc-msg (srcloc->string (build-source-location ID)))
  (define id (format "~s" (syntax->datum ID)))
  (define err-msg "multiple declaration")
  (define previous-ID (findf (curry bound-id=? ID) IDs))
  (define previous-ID-msg (format "~n  previously seen at line ~a:~a"
                                  (syntax-line previous-ID)
                                  (syntax-column previous-ID)))

  (raise (make-exn:name-clash
          (string-append srcloc-msg ": " id ": " err-msg previous-ID-msg)
          (current-continuation-marks)
          (list (syntax-taint ID)))))


;; Unknown type
(struct exn:type-unknown exn:fail:syntax ()
  #:extra-constructor-name make-exn:type-unknown
  #:transparent)

#;(define (raise-type-unknown B-TYPE)
  (define srcloc-msg (srcloc->string (build-source-location B-TYPE)))
  (define id (format "~s" (extract-exp-name B-TYPE)))
  (define err-msg "unknown type")
  (define elab-msg
    (format (string-append "~n  The expression elaborate to the type ~s"
                           "~n  But the expected type is ~s referring to declaration at ~a:~a"
                           "~n  in: ~.s")
            (syntax->datum GIVEN-B-TYPE)
            (syntax->datum EXPECTED-B-TYPE)
            (syntax-line EXPECTED-B-TYPE)
            (syntax-column EXPECTED-B-TYPE)
            (syntax->datum CTX)))

  (raise (make-exn:type-unknown
          (string-append srcloc-msg ": " id ": " err-msg elab-msg)
          (current-continuation-marks)
          (list (syntax-taint B-TYPE)))))

;; Type mismatch
(struct exn:type-mismatch exn:fail:syntax ()
  #:extra-constructor-name make-exn:type-mismatch
  #:transparent)

(define (raise-type-mismatch GIVEN-B-TYPE EXPECTED-B-TYPE CTX)
  (define srcloc-msg (srcloc->string (build-source-location CTX)))
  (define id (format "~s" (extract-exp-name CTX)))
  (define err-msg "type mismatch")
  (define elab-msg
    (format (string-append "~n  The expression elaborate to the type ~s"
                           "~n  But the expected type is ~s referring to declaration at ~a:~a"
                           "~n  in: ~.s")
            (syntax->datum GIVEN-B-TYPE)
            (syntax->datum EXPECTED-B-TYPE)
            (syntax-line EXPECTED-B-TYPE)
            (syntax-column EXPECTED-B-TYPE)
            (syntax->datum CTX)))

  (raise (make-exn:type-mismatch
          (string-append srcloc-msg ": " id ": " err-msg elab-msg)
          (current-continuation-marks)
          (list (syntax-taint CTX)))))


;; Utils

;; (Syntaxof a) -> (Syntaxof (Pairof (Syntaxof a) B-TYPE))
(define (get-τ stx)
  #`(#,stx #,(type-prop stx)))

;; (Syntaxof a) B-TYPE -> (Syntaxof a)
(define add-τ type-prop)

;; (: τ=? ((U OW-SCHEME B-TYPE) (U OW-SCHEME B-TYPE) [srcloc stx] -> Boolean))
(define (τ=? CURRENT-B-TYPE EXPECTED-B-TYPE #:srcloc [CTX (current-syntax-context)])
  (unless (bound-id=? CURRENT-B-TYPE EXPECTED-B-TYPE)
    (raise-type-mismatch CURRENT-B-TYPE EXPECTED-B-TYPE CTX)))

;; Ensures no name clash
(define (no-name-clash? stxs)
  (define get-name
    (syntax-parser
      #:literal-sets [keyword-lits]
      [(class name _ ...) #'name]
      [(field name _ ...) #'name]
      [(def (name _ ...) _) #'name]))

  (define names (stx-map get-name stxs))

  (cond
    ;; A duplicate name exists
    [(check-duplicate-identifier names)
     => (λ (name) (raise-name-clash name names))]
    ;; Everything is fine
    [else #t]))

;; (define (?> stx)
;;   (parameterize
;;       (;; - Map of locally bound variables.
;;        ;;   Γ : (HashTable Identifier B-TYPE)
;;        [Γ (make-immutable-id-hash)]

;;        ;; - Store of the current class type
;;        ;;   τ : B-TYPE
;;        [τ 'Unit]

;;        ;; - Set of types
;;        ;;   CS : (Listof B-TYPE)
;;        [CS (unbox meta:CS)]

;;        ;; - Map of fields
;;        ;;
;;        ;;   With the syntax #'(Class-type . Field-name) as key and the
;;        ;;   Field type as value.
;;        ;;
;;        ;;   FS : (Dict (Syntaxof (Pairof (B-TYPE Identifier))) B-TYPE))
;;        [FS (def/dict-map
;;              (syntax-parser [SCHEME:ow-scheme #'SCHEME.TYPE])
;;              (unbox meta:FS))]

;;        ;; - Map of definitions
;;        ;;
;;        ;;   With the syntax #'(Class-type Def-name (Def-arg-type ...)) as
;;        ;;   key and the Def return type as value.
;;        ;;
;;        ;;   DS : (Dict (Syntaxof (B-TYPE Identifier (Listof B-TYPE))) B-TYPE))
;;        [DS (def/dict-map
;;              (syntax-parser [SCHEME:ow-scheme #'SCHEME.TYPE])
;;              (unbox meta:DS))])
;;     (⊢p stx)))



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
