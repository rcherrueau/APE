#lang reader "reader.rkt" typed/racket/base/no-check

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.
;;
;; Desugaring syntax transformation (∗>)
;; - Introduce missing CPARAM.
;; - Transform let with multiple binding into nested lets of one
;;   binding.
;; - Expand short field access to canonical field access: (get-field
;;   *this* field).
;; - Expand types to ownership schemes.
;;
;; Environment:
;; - Γ is the set of locally bound variables
;;
;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects

(require (for-syntax racket/base)
         racket/contract/base
         racket/contract/region
         racket/function
         racket/list
         racket/match
         racket/string
         racket/syntax
         syntax/parse
         syntax/parse/define
         syntax/quote
         syntax/stx
         "utils.rkt"
         "definitions.rkt")

(module+ test (require rackunit))

(provide ∗>)


;; Transformation (∗>)
(: define-parser (Syntax -> Syntax))
(define-parser (∗> stx)

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #:Env
  ;; - Set of local bindings
  ;;   Γ : (Listof Identifier)
  ([Γ '()])

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Parser
  (∗p> stx))



;; Rules

(define-syntax define-rules
  (syntax-parser
    [(_ ID:id RULE:expr ...)
     #'(define ID
         (λ (stx)
           ;; (dbg stx #:ctx ID)
           (syntax-parse stx
             #:literal-sets [(keyword-lits #:at ID)
                             (expr-lits #:at ID)
                             (type-lits #:at ID)]
             RULE ...)))]))


;; A prog is a list of CLASS and one expression E
(: ∗p> (Syntax -> Syntax))
(define-rules ∗p>
  ;; Note: The `~!` eliminate backtracking. Hence, if the next
  ;; `fail-when` failed, it will not backtrack to try other cases.
  [(prog ~! CLASS:expr ... E:expr)
   #:with [*CLASS ...] (stx-map ∗c> #'(CLASS ...))
   #:with *E           (∗e> #'E)
   #@(prog *CLASS ... *E)])


;; A class is a NAME, an optional list of context parameters
;; CPARAM, and a list of fields and definitions.
(: ∗c> (Syntax -> Syntax))
(define-rules ∗c>
  [(class NAME:id [CPARAM:id ...] ~! FIELD/DEF:expr ...)
   #:with [*FIELD/DEF ...] (stx-map ∗f/d> #'(FIELD/DEF ...))
   #@(class NAME [CPARAM ...] *FIELD/DEF ...)]
  ;; Transforms a `class` without `CPARAM ...` into a `class` with.
  [(class NAME:id ~! FIELD/DEF:expr ...)
   (∗c> #@(class NAME [] FIELD/DEF ...))])


;; Fields and Defs
(: ∗f/d> (Syntax -> Syntax))
(define-rules ∗f/d>
  ;; A field declares one argument ARG (i.e., no initialization).
  ;;
  ;; (field ARG)
  ;; ∗>  (field NAME OW-SCHEME)
  ;; with
  ;;     OW-SCHEME := (ow-scheme TYPE OWNER CPARAMS)
  [(field ~! ARG:arg)
   #@(field ARG.NAME ARG.OW-SCHEME)]

  ;; A def (i.e., method) is a NAME, a list of arguments ARG, a
  ;; return type RET and the BODY of the def. The def binds ARG in
  ;; the BODY. The Γ, during transformation of BODY, contains the
  ;; local binding (i.e., `ARG.NAME ...`) of the def, plus `this`.
  ;;
  ;; (def (NAME ARG ... → RET) BODY)
  ;; ∗>  (def (NAME (A-NAME A-OW-SCHEME) ... RET-OW-SCHEME) *BODY)
  ;; with
  ;;     A-OW-SCHEME := (A-TYPE A-OWNER A-CPARAMS)
  [(def ~! (NAME:id ARG:arg ... . RET:rtype) BODY:expr)
   #:with [A-NAME ...]      #'(ARG.NAME ...)
   #:with [A-OW-SCHEME ...] #'(ARG.OW-SCHEME ...)
   #:with RET-OW-SCHEME     #'RET.OW-SCHEME
   #:with *BODY             (with-Γ #'(this A-NAME ...) (∗e> #'BODY))
   #@(def (NAME (~@ (A-NAME A-OW-SCHEME)) ... RET-OW-SCHEME) *BODY)])

;; TODO: test me!
;; (syntax-parse #'(-> owner/(type c1 c2))
;;   [(a ... . r:rtype) (check-stx=? #'r.OW-SCHEME #'(type owner (c1 c2)))]
;;   [_ (fail "#'(→ type)")])
;;
;; (syntax-parse #'(arg1 arg2 -> owner/(type c1 c2))
;;   [(a ... . r:rtype) (check-stx=? #'r.OW-SCHEME #'(type owner (c1 c2)))]
;;   [_ (fail "#'(→ type)")])

;; Expression
(: ∗e> (Syntax -> Syntax))
(define-rules ∗e>
  ;; A let binds a variables VAR with a type T to an expression E in
  ;; a BODY. During the transformation of BODY, Γ is extended with
  ;; the newly bound variable VAR.
  ;;
  ;; (let ([VAR : T E] ...) BODY)
  ;; ∗>  (let (VAR OW-SCHEME *E) (let... (...) *BODY)
  ;; with
  ;;     OW-SCHEME := (TYPE OWNER CPARAMS)
  [(let (ARG:arg+expr) ~! BODY:expr)
   #:with *E        (∗e> #'ARG.EXPR)
   #:with *BODY     (with-Γ (Γ-add #'ARG.VAR) (∗e> #'BODY))
   #@(let (ARG.VAR ARG.OW-SCHEME *E) *BODY)]
  ;; Transforms a `let` with multiple binding into multiple nested
  ;; `let`s with one unique binding (such as the previous let)
  [(let ~! (B1 BS ...) BODY:expr)
   (∗e> #@(let (B1) (let (BS ...) BODY)))]

  ;; A new takes the class type C-TYPE of the class to instantiate
  ;; (i.e., no constructor).
  ;;
  ;; (new C-TYPE)
  ;; ∗>  (new (TYPE OWNER CPARAMS))
  [(new ~! C-TYPE:type)
   #@(new C-TYPE.OW-SCHEME)]

  ;; A get-field takes an expression E that should reduce to an
  ;; object and the name of the field FNAME to get on that object.
  ;;
  ;; (get-field E FNAME)
  ;; ∗>  (get-field *E FNAME)
  [(get-field ~! E:expr FNAME:id)
   #:with *E (∗e> #'E)
   #@(get-field *E FNAME)]

  ;; A set-field! takes an expression E that should reduce to an
  ;; object, the name of the field FNAME to change the value of, and
  ;; the BODY of the new value.
  ;;
  ;; (set-field! E FNAME BODY)
  ;; ∗>  (set-field! *E FNAME *BODY)
  [(set-field! ~! E:expr FNAME:id BODY:expr)
   #:with *E    (∗e> #'E)
   #:with *BODY (∗e> #'BODY)
   #@(set-field! *E FNAME *BODY)]

  ;; A send takes an expression E that should reduce to an object,
  ;; the name of the def DNAME to call on that object, and a list of
  ;; expressions `E-ARG ...` to pass as arguments to the def.
  ;;
  ;; (send E DNAME E-ARG ...)
  ;; ∗>  (send *E DNAME *E-ARG)
  [(send ~! E:expr DNAME:id E-ARG:expr ...)
   #:with *E           (∗e> #'E)
   #:with [*E-ARG ...] (stx-map ∗e> #'(E-ARG ...))
   #@(send *E DNAME *E-ARG ...)]

  ;; An identifier is either:
  ;;
  ;;;; A local binding (from a def or let). It include the `this`
  ;;;; keyword in the case we are in the context of a def.
  [ID:id #:when (Γ-member? #'ID)
    this-syntax]
  ;;;; The debug placeholder ???
  [???
   this-syntax]
  ;;;; A class level binding (no binder). In that case, it presumably
  ;;;; refers to a field of the current class: A sort of shortcut for
  ;;;; (get-field this id) -- i.e., `id` instead of `this.id` in Java
  ;;;; world. E.g.,
  ;;;;
  ;;;; 1 (class C
  ;;;; 2   (field [id : A])
  ;;;; 3   (def (get-id → A) id))
  ;;;;
  ;;;; With line 3, a shortcut for
  ;;;; > (def (get-id → A) (get-field this id))
  ;;;;
  ;;;; We remove it, so the desugared syntax contains no class level
  ;;;; binding.
  ;;;; ID ∗> *(get-field this ID)
  [ID:id
   (∗e> #@(get-field this ID))])


;; Environment -- Manage local binding (Γ)

;; Is VAR bounded in Γ?
(: Γ-member? (Identifier -> Boolean))
(define (Γ-member? VAR)
  (when (not (list? (Γ)))
    (error 'Γ-member?
           (string-append
            "Γ expected a (listof syntax?) but ~a was given."
            "~n  Is expression wrapped in `with-Γ`?")
           (Γ)))

  (if (findf (curry bound-id=? VAR) (Γ)) #t #f))

;; Add a VAR to Γ.
;; : Identifier -> List Identifier
(: Γ-add (Identifier -> Void))
(define (Γ-add VAR)
  (if (Γ-member? VAR)
      (Γ)
      (cons VAR (Γ))))

;; Make `the-Γ` a new value for Γ in the context of
;; STX.
;; : (U (List VAR) #'(VAR ...)) (-> STX) -> STX
(define (private:with-Γ the-Γ thunk-E)
  (define listof-id? (listof identifier?))
  (parameterize
    ([Γ (cond
          [(listof-id? the-Γ) the-Γ]
          [(and (syntax? the-Γ) (stx->list the-Γ)) => identity]
          [else (raise-argument-error
                 'with-Γ "(or/c syntax? (listof syntax?))" the-Γ)])])
    (thunk-E)))

(define-syntax-parser with-Γ
  ;; Automatically create the `thunk` around E expressions
  [(_ THE-Γ E:expr ...) #'(private:with-Γ THE-Γ (thunk E ...))])

(module+ test
  (define test:Γ (list #'foo #'bar))

  (define-test-suite Γ-tests
    (check-true  (with-Γ #'(foo bar) (Γ-member? #'foo)))
    (check-true  (with-Γ #'(foo bar) (Γ-member? #'bar)))
    (check-true (with-Γ test:Γ (Γ-member? #'foo)))
    (check-false (with-Γ test:Γ (Γ-member? #'baz)))
    (check-exn exn:fail? (thunk (Γ-member? #'baz))
               "expression unwrapped in `with-Γ` is not valid")

    (with-Γ test:Γ
      (with-Γ (Γ-add #'baz) (check-true  (Γ-member? #'baz)))
      (check-false (Γ-member? #'baz)))))


;; Syntax for type and arg

;; TODO: Remove this
(define-literal-set type-lits
  ;; Don't consider :, →, and / as patterns
  #:datum-literals (: → ->)
  ())

(define-syntax-class type
  #:description (string-append
                 "a ownership type is one of the form: "
                 "~n- owner/type"
                 "~n- owner/(type param ...)"
                 "~n- type -- owner is implictly world"
                 "~n- (type param ...) -- owner is implicitly world.")
  #:attributes [TYPE OWNER CPARAMS OW-SCHEME]
  #:commit
  ;; owner/type
  (pattern T:id #:when (is-stx-owner/type? #'T)
           #:with [OWNER . TYPE] (owner/type->OWNER.TYPE #'T)
           #:with CPARAMS #'()
           #:with OW-SCHEME (syntax/loc #'T (TYPE OWNER CPARAMS)))
  ;; (owner/(type ctx-params ...))
  (pattern (O/:id (TYPE:id PARAMS:id ...+))
           #:when (is-stx-owner/? #'O/)
           #:with OWNER (owner/->OWNER #'O/)
           #:with CPARAMS #'(PARAMS ...)
           #:with OW-SCHEME (syntax/loc #'T (TYPE OWNER CPARAMS)))
  ;; type -- owner is implicitly world
  (pattern T:id
           #:with OWNER #'world
           #:with TYPE #'T
           #:with CPARAMS #'()
           #:with OW-SCHEME (syntax/loc #'T (TYPE OWNER CPARAMS)))
  ;; (type ctx params ...) -- owner is implicitly world
  (pattern (T:id PARAMS:id ...+)
           #:with OWNER #'world
           #:with TYPE #'T
           #:with CPARAMS #'(PARAMS ...)
           #:with OW-SCHEME (syntax/loc #'T (TYPE OWNER CPARAMS))))

(module+ test
  (define-test-suite type-parse
    (test-parse #'owner/type t:type
      (check-stx=? #'t.TYPE      #'type)
      (check-stx=? #'t.OWNER     #'owner)
      (check-stx=? #'t.CPARAMS   #'())
      (check-stx=? #'t.OW-SCHEME #'(type owner ())))

    (test-parse #'owner/ty/pe t:type
      (check-stx=? #'t.TYPE      #'ty/pe)
      (check-stx=? #'t.OWNER     #'owner)
      (check-stx=? #'t.CPARAMS   #'())
      (check-stx=? #'t.OW-SCHEME #'(ty/pe owner ())))

    (test-parse #'(owner/(type c1 c2)) t:type
      (check-stx=? #'t.TYPE      #'type)
      (check-stx=? #'t.OWNER     #'owner)
      (check-stx=? #'t.CPARAMS   #'(c1 c2))
      (check-stx=? #'t.OW-SCHEME #'(type owner (c1 c2))))

    (test-parse #'type t:type
      (check-stx=? #'t.TYPE      #'type)
      (check-stx=? #'t.OWNER     #'world)
      (check-stx=? #'t.CPARAMS   #'())
      (check-stx=? #'t.OW-SCHEME #'(type world ())))

    (test-parse #'(type c1 c2) t:type
      (check-stx=? #'t.TYPE      #'type)
      (check-stx=? #'t.OWNER     #'world)
      (check-stx=? #'t.CPARAMS   #'(c1 c2))
      (check-stx=? #'t.OW-SCHEME #'(type world (c1 c2))))

    (check-ill-parsed #'(owner/type (c1 c2)) _:type)
    (check-ill-parsed #'(type (c1 c2)) _:type)
    (check-ill-parsed #'(ow/ner/{type c1 c2}) _:type
        #:msg "Putting a `/` in owner name is not a valid syntax")))


(define-syntax-class rtype
  #:description "a return type"
  #:datum-literals [-> →]
  #:commit
  (pattern (-> T:type)   #:attr OW-SCHEME #'T.OW-SCHEME)
  (pattern (→ T:type)    #:attr OW-SCHEME #'T.OW-SCHEME)
  ;; The `.' removes the need of parentheses that surround the `T'.
  (pattern (-> . T:type) #:attr OW-SCHEME #'T.OW-SCHEME)
  (pattern (→ . T:type)  #:attr OW-SCHEME #'T.OW-SCHEME))

(module+ test
  (define-test-suite rtype-parse
    (for ([-> (list #'→ #'->)])
      (test-parse #`(#,-> owner/type) r:rtype
        (check-stx=? #'r.OW-SCHEME #'(type owner ())))

      (test-parse #`(#,-> owner/{type c1 c2}) r:rtype
        (check-stx=? #'r.OW-SCHEME #'(type owner (c1 c2))))

      (test-parse #`(#,-> (owner/{type c1 c2})) r:rtype
        (check-stx=? #'r.OW-SCHEME #'(type owner (c1 c2))))

      (test-parse #`(#,-> type) r:rtype
        (check-stx=? #'r.OW-SCHEME #'(type world ())))

      (test-parse #`(#,-> (type c1 c2)) r:rtype
        (check-stx=? #'r.OW-SCHEME #'(type world (c1 c2)))))))


(define-syntax-class arg
  #:description "an argument with its type"
  #:datum-literals [:]
  #:commit
  (pattern (NAME:id : T:type)
           #:attr OW-SCHEME #'T.OW-SCHEME)
  ;; The `.' removes the need of parentheses that surround the `T'.
  (pattern (NAME:id : ~! . T:type)
           #:attr OW-SCHEME #'T.OW-SCHEME))

(module+ test
  (define-test-suite arg-parse
    (test-parse #'(name : owner/type) a:arg
      (check-stx=? #'a.OW-SCHEME #'(type owner ())))

    (test-parse #'(name : owner/{type c1 c2}) a:arg
      (check-stx=? #'a.OW-SCHEME #'(type owner (c1 c2))))

    (test-parse #'(name : (owner/{type c1 c2})) a:arg
      (check-stx=? #'a.OW-SCHEME #'(type owner (c1 c2))))

    (test-parse #'(name : type) a:arg
      (check-stx=? #'a.OW-SCHEME #'(type world ())))

    (test-parse #'(name : (type c1 c2)) a:arg
      (check-stx=? #'a.OW-SCHEME #'(type world (c1 c2))))

    (check-ill-parsed #'(name {owner/type}) _:arg)
    (check-ill-parsed #'(name : own/er/{type (c1 c2)}) _:arg
      #:msg "Putting a `/` in owner name is not a valid syntax")))


(define-syntax-class arg+expr
  #:description "an argument with its type and a binding"
  #:datum-literals [:]
  #:attributes [VAR OW-SCHEME EXPR]
  #:commit
  (pattern (VAR:id : T:type E:expr)
           #:attr EXPR #'E
           #:attr OW-SCHEME #'T.OW-SCHEME)
  (pattern (VAR:id : O/:id T:type E:expr)
           #:when (and (is-stx-owner/? #'O/) (bound-id=? #'T.OWNER #'world))
           #:cut
           #:with OWNER (owner/->OWNER #'O/)
           #:attr EXPR #'E
           #:attr OW-SCHEME (syntax/loc #'T (T.TYPE OWNER T.CPARAMS))))

(module+ test
  (define-test-suite arg+expr-parse
    (test-parse #'(name : owner/type (expr)) a:arg+expr
       (check-stx=? #'a.VAR #'name)
       (check-stx=? #'a.OW-SCHEME #'(type owner ()))
       (check-stx=? #'a.EXPR #'(expr)))

    (test-parse #'(name : owner/{type c1 c2} (expr)) a:arg+expr
      (check-stx=? #'a.VAR #'name)
      (check-stx=? #'a.OW-SCHEME #'(type owner (c1 c2)))
      (check-stx=? #'a.EXPR #'(expr)))

    (test-parse #'(name : (owner/{type c1 c2}) (expr)) a:arg+expr
      (check-stx=? #'a.VAR #'name)
      (check-stx=? #'a.OW-SCHEME #'(type owner (c1 c2)))
      (check-stx=? #'a.EXPR #'(expr)))

    (test-parse #'(name : type (expr)) a:arg+expr
      (check-stx=? #'a.VAR #'name)
      (check-stx=? #'a.OW-SCHEME #'(type world ()))
      (check-stx=? #'a.EXPR #'(expr)))

    (test-parse #'(name : (type c1 c2) (expr)) a:arg+expr
      (check-stx=? #'a.VAR #'name)
      (check-stx=? #'a.OW-SCHEME #'(type world (c1 c2)))
      (check-stx=? #'a.EXPR #'(expr)))

    (check-ill-parsed #'(name : owner/type expr1 expr2) a:arg+expr)))


;; Utils

;; Returns #t if a syntax object is of the form #'owner/type, or #f
;; otherwise.
(: is-stx-owner/type? (Syntax -> Boolean))
(define (is-stx-owner/type? stx)
  (and (identifier? stx)
       (string-contains? (symbol->string (syntax-e stx)) "/")))

;; Split a #'owner/type syntax object on the first `/` and returns a
;; syntax pair #'(owner . type).
(: owner/type->OWNER.TYPE
   (Syntax -> (Syntaxof (Pairof Identifier Identifier))))
(define (owner/type->OWNER.TYPE stx)
  (match-define (list owner-str type-str ...)
    (string-split (symbol->string (syntax-e stx)) "/"))

  (let ([OWNER (format-id stx #:source stx
                          "~a" owner-str)]
        [TYPE  (format-id stx #:source stx
                          "~a" (string-join type-str "/"))])
    #`(#,OWNER . #,TYPE)))

;; Returns #t if a syntax object is of the form #'owner/, or #f
;; otherwise.
(: is-stx-owner/? (Syntax -> Boolean))
(define (is-stx-owner/? stx)
  (and (identifier? stx)
       (let* ([id-str (symbol->string (syntax-e stx))]
              [id-str-length (string-length id-str)]
              [id-str-1 (substring id-str 0 (- id-str-length 2))])
         ;; Test there is only a "/" at the end of the identifier
         (and (string-suffix? id-str "/")
              (not (string-contains? id-str-1 "/"))))))

;; Trims a #'owner/ syntax object on the last `/` and returns a
;; syntax #'owner.
(: owner/->OWNER (Identifier -> Identifier))
(define (owner/->OWNER stx)
  (define owner-str
    (string-trim (symbol->string (syntax-e stx))
                 "/"
                 #:left? #f))

  (format-id stx #:source stx "~a" owner-str))

(module+ test
  (define-test-suite owner/type-stx-split
    (check-true  (is-stx-owner/type? #'owner/type))
    (check-true  (is-stx-owner/type? #'owner/t/type))
    (check-false (is-stx-owner/type? #'ownertype))

    (check-stx=? (owner/type->OWNER.TYPE #'owner/type)
                 #'(owner . type))
    (check-stx=? (owner/type->OWNER.TYPE #'owner/ty/pe)
                 #'(owner . ty/pe)))

  (define-test-suite owner/-stx-split
    (check-true (is-stx-owner/? #'owner/))
    (check-false (is-stx-owner/? #'own/er/))
    (check-false (is-stx-owner/? #'owner))

    (check-stx=? (owner/->OWNER #'owner/) #'owner)))


;; Tests
(module+ test
  (require rackunit/text-ui)
  (run-tests Γ-tests)
  (run-tests owner/type-stx-split)
  (run-tests owner/-stx-split)
  (run-tests type-parse)
  (run-tests rtype-parse)
  (run-tests arg-parse)
  (run-tests arg+expr-parse)
  )
