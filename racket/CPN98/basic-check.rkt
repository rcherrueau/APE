#lang typed/racket/base/no-check

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
                    [dict-map      def/dict-map]
                    [dict-keys     def/dict-keys])
         (prefix-in env: (submod "env.rkt" basic-check)))

(module+ test (require rackunit))

(provide ?>)


;; Transformation (?>)

(define-parser (?> stx)
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #:Env
  (;; Map of locally bound variables.
   ;; TODO:
   ;; [Γ : Id~>B-TYPE
   ;;    #:mk   env:make-Γ   ;; Maybe something à la datatype
   ;;    #:init '()
   ;;    #:partial-app ([env:Γ-member? Γ-member?]
   ;;                   [env:Γ-add     Γ-add]
   ;;                   [env:Γ-ref     Γ-ref])]
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
       env:make-CS (unbox meta:CS)
       #:partial-app ([env:CS-member? CS-member?])]

   ;; Map of fields
   ;;
   ;; With the syntax #'(Class-type . Field-name) as key and the Field
   ;; type as value.
   [FS : (HashTable
          (Syntaxof (Pairof B-TYPE Identifier)) ;; #'(Class-type . Field-name)
          B-TYPE)                               ;; #'Field-type
       env:make-FS
       (meta-map  ;; Instantiate ows in values
        (syntax-parser [SCHEME:ow-scheme #'SCHEME.TYPE])
        (unbox meta:FS))
       #:partial-app ([env:FS-member? FS-member?]
                      [env:FS-ref FS-ref])]

   ;; Map of definitions
   ;;
   ;; With the syntax #'(Class-type Def-name (Def-arg-type ...)) as
   ;; key and the Def return type as value.
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
        (unbox meta:DS))
       #:partial-app ([env:DS-member? DS-member?]
                      [env:DS-ref DS-ref]
                      [env:DS-domain DS-domain])])

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Parse
  (⊢p stx))


;; Rules

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


;; (check-true (no-name-clash? #'((class foo (field foo)) (class bar (field bar)))))

;; (check-true (no-name-clash? #'((class foo (field foo)) (class bar (field foo))))
;;             "Two field could have the same name in different class")
;; (check-true (no-name-clash? #'((class foo (field foo) (def foo (foo))))))
;; ;; (check-true (no-name-clash? #'((class foo (field foo) (field foo)))))

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
   #:when (or (FS-member? #'(t . FNAME)) (raise-unknown-field #'FNAME #'t))
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
   ;; Check P,Γ ⊢e PARAM ... ≫ (?PARAM : t) ...
   #:with [(?PARAM t-param) ...] (stx-map (∘ get-τ ⊢e) #'(PARAM ...))
   ;; Check (t DNAME (t-param ...)) ∈ dom(DS)
   ;;;; The method DNAME with parameters (t-param ...) is defined in
   ;;;; the class t.
   #:with DS-key #'(t DNAME (t-param ...))
   #:when (or (DS-member? #'DS-key) (raise-def-error #'DS-key))
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e (send E DNAME PARAM ...) ≫
   ;;          (send (?E : t) DNAME ?PARAM ...) : DS(t DNAME PARAM ...)
   (add-τ this-syntax (DS-ref #'DS-key))]

  ;; [let]
  [(let ~! (VAR-NAME VAR-SCHEME:ow-scheme E) BODY)
   #:when (⊢τ #'VAR-SCHEME.TYPE)
   ;; Check  P,Γ ⊢e E ≫ ?E : VAR-OW-SCHEME
   #:with [?E t] (get-τ (with-Γ (Γ-add #'(??? . VAR-SCHEME.TYPE))
                          (⊢e #'E)))
   #:when (or (τ=? #'t #'VAR-SCHEME.TYPE)
              (raise-type-mismatch #'t #'VAR-SCHEME.TYPE #'?E))
   ;; Check P,Γ{VAR-NAME: VAR-OW-SCHEME} ⊢e BODY ≫ ?BODY : t
   #:with [_ t-body] (get-τ (with-Γ (Γ-add #'(VAR-NAME . VAR-SCHEME.TYPE))
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
  [t #:when (or (CS-member? #'t) (raise-type-unknown #'t))
     this-syntax])


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
    (match-define (list _ _ EXPECTED-DEF-ARGs) expected-def)
    (define expected-def-arity (length EXPECTED-DEF-ARGs))
    (raise-arity-error LOOKED-DEF-NAME expected-def-arity LOOKED-DEF-ARGs))

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

(define (raise-type-unknown B-TYPE)
  (define srcloc-msg (srcloc->string (build-source-location B-TYPE)))
  (define id (format "~s" (syntax->datum B-TYPE)))
  (define err-msg "unknown type in this scope")

  (raise (make-exn:type-unknown
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
  (log-lang-debug "Original error in ~.s" CTX)
  (define CTX-SURFACE (syntax-property CTX 'surface))
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

  (log-lang-debug "Original error in ~.s" CTX)

  (raise (make-exn:type-mismatch
          (string-append srcloc-msg ": " id ": " err-msg elab-msg)
          (current-continuation-marks)
          (list (syntax-taint CTX)))))

;; Def arity error
(struct exn:arity-error exn:fail:syntax ()
  #:extra-constructor-name make-exn:arity-error
  #:transparent)

;; (: raise-arity-error (Identifier Integer (Listof B-TYPE) STX -> exn:arity-error))
(define (raise-arity-error def-name expected-args-size given-args [context #f])
  (define CTX (or context (current-syntax-context)))
  (log-lang-debug "Original error in ~.s" CTX)
  (define CTX-SURFACE (syntax-property CTX 'surface))
  (define srcloc-msg (srcloc->string (build-source-location CTX-SURFACE)))
  (define id (format "~s" (extract-exp-name def-name)))
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
            (syntax-line def-name)
            (syntax-column def-name)
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
  (log-lang-debug "Original error in ~.s" CTX)
  (define CTX-SURFACE (syntax-property CTX 'surface))
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
  (log-lang-debug "Original error in ~.s" CTX)
  (define CTX-SURFACE (syntax-property CTX 'surface))
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

;; Ensures no name clash
(define (no-name-clash? stxs)
  ;; Extract names from a `class`, `field` or `def`
  (define get-name
    (syntax-parser
      #:datum-literals [class field def]
      [(class name:id _ ...) #'name]
      [(field name:id _ ...) #'name]
      [(def (name:id _ ...) _) #'name]))

  ;; Extract names from `stxs`
  (define names (stx-map get-name stxs))

  (cond
    ;; A duplicate name exists
    [(check-duplicate-identifier names)
     => (λ (name) (raise-name-clash name names))]
    ;; Everything is fine
    [else #t]))

(module+ test
  (define-test-suite utils
    ;; Check
    (check-stx=? (get-τ (add-τ #'foo #'bar)) #'(foo bar))
    (check-stx=? (get-τ (add-τ (add-τ #'foo #'bar) #'baz)) #'(foo baz))
    (check-stx=? (get-τ #'foo) #'(foo #f))  ;; TODO: raise an exception?

    ;; Check name clash
    (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((clazz foo))))
               "`clazz` is not a `class`")
    (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((class (foo)))))
               "`(foo)` is not a valid name")
    (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((filed foo))))
               "`filed` is not a `field`")
    (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((field (foo)))))
               "`(foo)` is not a valid name")
    (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((dEf (foo) ???))))
               "`dEf` is not a `def`")
    (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((def ((foo)) ???))))
               "`(foo)` is not a valid name")

    (check-true (no-name-clash? #'((class foo) (class bar))))
    (check-true (no-name-clash? #'((field foo) (field bar))))
    (check-true (no-name-clash? #'((def (foo) ???) (def (bar) ???))))

    (check-exn exn:name-clash? (thunk (no-name-clash? #'((class foo) (class foo)))))
    (check-exn exn:name-clash? (thunk (no-name-clash? #'((field foo) (field foo)))))
    (check-exn exn:name-clash? (thunk (no-name-clash? #'((def (foo) ???) (def (foo) ???)))))
    )
  )

;; Tests
(module+ test
  (require rackunit/text-ui
           (prefix-in env: (submod "env.rkt" basic-check test)))
  (provide basic-check-tests)

  (define basic-check-tests
    (test-suite
     "Tests for basic checks"
     ;; Check env
     env:CS-tests
     env:Γ-tests
     env:FS-tests
     env:DS-tests
     ;; Check utils
     utils
     ))

  (run-tests basic-check-tests)
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
