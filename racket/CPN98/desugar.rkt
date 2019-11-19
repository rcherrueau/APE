#lang racket/base

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
;;   this field).
;; - Expand types to ownership schemes.
;;
;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects
;;
;; Environment:
;; - Γ is the set of locally bounded variables

(require (for-syntax racket/base)
         racket/contract/base
         racket/function
         racket/list
         ;; racket/set
         ;; syntax/id-set
         syntax/parse
         syntax/parse/define
         syntax/quote
         syntax/stx
         "utils.rkt"
         "definitions.rkt"
         )

(provide ∗>)


;; ∗> :: stx -> stx
(define-parser ∗>
  #:literal-sets [keyword-lits expr-lits type-lits]

  ;; A prog is a list of CLASS and one expression E.
  ;;
  ;; (prog CLASS ... E)
  ;; ∗>  (prog *CLASS ... *E)
  ;;
  ;; Note: The `~!` eliminate backtracking. Hence, if the next
  ;; `fail-when` failed, it will not backtrack and try other cases.
  [(prog ~! CLASS:expr ... E:expr)
   ;; #:and (~do (dbg this-syntax))
   #:with [*CLASS ...] (stx-map ∗> #'(CLASS ...))
   #:with *E           (∗> #'E)
   (stx/this-loc (prog *CLASS ... *E))]

  ;; A class is a NAME, an optional list of context parameters
  ;; CPARAM, and a list of fields and definitions.
  ;;
  ;; (class NAME (CPARAM ...)? FIELD ... DEF ...)
  ;; ∗>  (class NAME (CPARAM ...) *FIELD ... *DEF ...)
  [(class NAME:id [CPARAM:id ...] ~! FIELD/DEF:expr ...)
   ;; #:and (~do (dbg this-syntax))
   #:with [*FIELD/DEF ...] (stx-map ∗> #'(FIELD/DEF ...))
   (stx/this-loc (class NAME [CPARAM ...] *FIELD/DEF ...))]
  ;; Transforms a `class` without `CPARAM ...` into a `class` with.
  [(class ~! NAME FIELD/DEF ...)
   ;; #:and (~do (dbg this-syntax))
   (∗> (stx/this-loc (class NAME [] FIELD/DEF ...)))]

  ;; A field declares one argument ARG (i.e., no initialization).
  ;;
  ;; (field ARG)
  ;; ∗>  (field NAME OW-SCHEME)
  ;; with
  ;;     OW-SCHEME := (ow-scheme TYPE OWNER CPARAMS)
  [(field ~! ARG:arg)
   ;; #:and (~do (dbg this-syntax))
   #:with NAME      #'ARG.NAME
   #:with OW-SCHEME (type∗>ow-scheme #'ARG.T)
   (stx/this-loc (field NAME OW-SCHEME))]

  ;; A def (i.e., method) is a NAME, a list of arguments ARG, a return
  ;; type RET and the BODY of the def. The def binds ARG in the BODY.
  ;; The Γ, during transformation of BODY, contains the local
  ;; binding (i.e., `ARG.NAME ...`) of the def, plus `this`.
  ;;
  ;; (def (NAME ARG ... → RET) BODY)
  ;; ∗>  (def (NAME (A-NAME A-OW-SCHEME) ... RET-OW-SCHEME) *BODY)
  ;; with
  ;;     α-OW-SCHEME := (ow-scheme α-TYPE α-OWNER α-CPARAMS)
  [(def ~! (NAME:id ARG:arg ... → RET:type) BODY:expr)
   ;; #:and (~do (dbg this-syntax))
   #:with [A-NAME ...]      #'(ARG.NAME ...)
   #:with [A-OW-SCHEME ...] (stx-map type∗>ow-scheme #'(ARG.T ...))
   #:with RET-OW-SCHEME     (type∗>ow-scheme #'RET)
   #:with *BODY             (with-Γ #'(this A-NAME ...) (∗> #'BODY))
   (stx/this-loc (def (NAME (~@ (A-NAME A-OW-SCHEME)) ... RET-OW-SCHEME) *BODY))]

  ;; A let binds a variables VAR with a type T to an expression E in a
  ;; BODY. During the transformation of BODY, Γ is extended with the
  ;; newly bound variable VAR.
  ;;
  ;; (let ([VAR : T E] ...) BODY)
  ;; ∗>  (let (VAR OW-SCHEME *E) (let... (...) *BODY)
  ;; with
  ;;     OW-SCHEME := (ow-scheme TYPE OWNER CPARAMS)
  [(let ([VAR:id : T:type E:expr]) ~! BODY:expr)
   ;; #:and (~do (dbg this-syntax))
   #:with OW-SCHEME (type∗>ow-scheme #'T)
   #:with *E        (∗> #'E)
   #:with *BODY     (with-Γ (Γ-add #'VAR) (∗> #'BODY))
   (stx/this-loc (let (VAR OW-SCHEME *E) *BODY))]
  ;; Transforms a `let` with multiple binding into multiple nested
  ;; `let`s with one unique binding (such as the previous let)
  [(let ~! (B1 BS ...) BODY:expr)
   (∗> (stx/this-loc (let (B1) (let (BS ...) BODY))))]

  ;; A new takes the class type C-TYPE of the class to instantiate
  ;; (i.e., no constructor).
  ;;
  ;; (new C-TYPE)
  ;; ∗>  (new OW-SCHEME)
  ;; with
  ;;     OW-SCHEME := (ow-scheme TYPE OWNER CPARAMS)
  [(new ~! C-TYPE:type)
   ;; #:and (~do (dbg this-syntax))
   #:with OW-SCHEME (type∗>ow-scheme #'C-TYPE)
   (stx/this-loc (new OW-SCHEME))]

  ;; A get-field takes an expression E that should reduce to an
  ;; object and the name of the field FNAME to get on that object.
  ;;
  ;; (get-field E FNAME)
  ;; ∗>  (get-field *E FNAME)
  [(get-field ~! E:expr FNAME:id)
   ;; #:and (~do (dbg this-syntax))
   #:with *E (∗> #'E)
   (stx/this-loc (get-field *E FNAME))]

  ;; A set-field! takes an expression E that should reduce to an
  ;; object, the name of the field FNAME to change the value of, and
  ;; the BODY of the new value.
  ;;
  ;; (set-field! E FNAME BODY)
  ;; ∗>  (set-field! *E FNAME *BODY)
  [(set-field! ~! E:expr FNAME:id BODY:expr)
   ;; #:and (~do (dbg this-syntax))
   #:with *E    (∗> #'E)
   #:with *BODY (∗> #'BODY)
   (stx/this-loc (set-field! *E FNAME *BODY))]

  ;; A send takes an expression E that should reduce to an object,
  ;; the name of the def DNAME to call on that object, and a list of
  ;; expressions `E-ARG ...` to pass as arguments to the def.
  ;;
  ;; (send E DNAME E-ARG ...)
  ;; ∗>  (send *E DNAME *E-ARG)
  [(send ~! E:expr DNAME:id E-ARG:expr ...)
   ;; #:and (~do (dbg this-syntax))
   #:with *E           (∗> #'E)
   #:with [*E-ARG ...] (stx-map ∗> #'(E-ARG ...))
   (stx/this-loc (send *E DNAME *E-ARG ...))]

  ;; An identifier is either:
  ;;
  ;; - A local binding (from a def or let). It include the `this`
  ;;   keyword in the case we are in the context of a def.
  [ID:id #:when (Γ-member? #'ID)
   ;; #:and (~do (dbg this-syntax))
   this-syntax]
  ;; - A class level binding (no binder). In that case, it presumably
  ;;   refers to a field of the current class: A sort of shortcut for
  ;;   (get-field this id) -- i.e., `id` instead of `this.id` in Java
  ;;   world. E.g.,
  ;;
  ;;   1 (class C
  ;;   2   (field [id : A])
  ;;   3   (def (get-id → A) id))
  ;;
  ;;   With line 3, a shortcut for
  ;;   > (def (get-id → A) (get-field this id))
  ;;
  ;;   We remove it, so the desugared syntax contains no class level
  ;;   binding.
  ;;   ID ∗> *(get-field this ID)
  [ID:id
   ;; #:and (~do (dbg this-syntax))
   (∗> (stx/this-loc (get-field this ID)))]
  )


;; Environment

;; ~~~~~~~~~~~~~~~~~~~~
;; Manage local binding

;; Set Γ of local bindings.
;; : -> (List VAR)
(define Γ (make-parameter '()))

;; Is VAR bounded in Γ?
;; : VAR -> Boolean
(define (Γ-member? VAR)
  (if (findf (curry bound-id=? VAR) (Γ)) #t #f))

;; Add a VAR to Γ.
;; : VAR -> List VAR
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
          [else
           (raise-argument-error
            'with-Γ "(or/c syntax? (listof syntax?))" the-Γ)])])
    ;; (dbg (bound-id-set->list (Γ)))
    (thunk-E)))

(define-syntax-parser with-Γ
  ;; Automatically create the `thunk` around E expressions
  [(_ THE-Γ E:expr ...) #'(private:with-Γ THE-Γ (thunk E ...))])


;; Tests
(module+ test
  (require rackunit
           rackunit/text-ui)

  (define test:Γ (list #'foo #'bar))

  (run-tests
   (test-suite
    "Tests for basic checks transformation (?>)"

    ;; ----------------------------------------------------------------
    ;; Bound identifiers
    (test-case "Bound identifiers"

      ;; Primitives
      (check-true  (with-Γ #'(foo bar) (Γ-member? #'foo)))
      (check-false (with-Γ #'(foo bar) (Γ-member? #'baz)))
      (check-true  (with-Γ test:Γ (Γ-member? #'foo)))
      (check-false (with-Γ test:Γ (Γ-member? #'baz)))

      (with-Γ #'(foo bar)
        (check-true  (with-Γ (Γ-add #'baz) (Γ-member? #'baz)))
        (check-false (with-Γ (Γ-add #'baz) (Γ-member? #'xyzzy))))

      ;; Syntax
      )

    ;;   (check-not-exn
    ;;    (thunk (with-Γ #'(id) (?> #'id)))
    ;;    "id is bounded if it exists in Γ")
    ;;   (check-exn
    ;;    #rx"unbound identifier.+?in: id"
    ;;    (thunk (?> #'id))
    ;;    "id is unbound if it does not exist in Γ")

    ;;   ;; def
    ;;   (check-not-exn
    ;;    (thunk (?> #`(def (x (id #,τ✔) #,τ✔) id)))
    ;;    "id is bounded if it is a parameter of the def")
    ;;   (check-not-exn
    ;;    (thunk (?> #`(def (x (id #,τ✔) #,τ✔) this)))
    ;;    "this is bounded in a def")
    ;;   (check-exn
    ;;    #rx"unbound identifier.+?in: id"
    ;;    (thunk (?> #`(def (x #,τ✔) id)))
    ;;    "id is unbound if it is not a parameter of the def")

    ;;   ;; let
    ;;   (check-not-exn
    ;;    (thunk (?> #`(let (id #,τ✔ #,EXPR:τ✔) id)))
    ;;    "let binder bound the id")
    ;;   (check-exn
    ;;    #rx"unbound identifier.+?in: id"
    ;;    (thunk (?> #`(let (x #,τ✔ id) x)))
    ;;    "id is unbound in the binder of a let if it does not exist in Γ")
    ;;   (check-exn
    ;;    #rx"unbound identifier.+?in: id"
    ;;    (thunk (?> #`(let (x #,τ✔ #,EXPR:τ✔) id)))
    ;;    "id is unbound in the body of a let if it does not exist in Γ")

    ;;   ;; FIXME: Right now, unbound ids in class are forgotten. Rather, an
    ;;   ;; exception should be raised.
    ;;   ;; (check-exn
    ;;   ;;           #rx"unbound identifier.+?in: id"
    ;;   ;;           (thunk (?> #'(class X []  id))))
    ;;   (check-exn #rx"unbound identifier.+?in: id"
    ;;             (thunk (?> #`(get-field id _))))

    ;;   (check-exn #rx"unbound identifier.+?in: id"
    ;;              (thunk (?> #`(set-field! id _ _)))
    ;;              "id is bound in the receiver object of a set-field!")

    ;;   (check-exn #rx"unbound identifier.+?in: id"
    ;;              (thunk (?> #`(set-field! #,EXPR:τ✔ _ id)))
    ;;              "id is bound in the body of a set-field!")

    ;;   (check-exn #rx"unbound identifier.+?in: id"
    ;;              (thunk (?> #`(send id _)))
    ;;              "id is bound in the receiver object of a send")

    ;;   (check-exn #rx"unbound identifier.+?in: id"
    ;;              (thunk (?> #`(send #,EXPR:τ✔ _ id)))
    ;;              "id is bound in the argument of a send"))


    ;; ;; ----------------------------------------------------------------
    ;; ;; Unknown Type
    ;; (test-case "Known & Unknown Types"
    ;;   (check-not-exn
    ;;    (thunk (?> #`(prog (class X  []) (new (ow-scheme X x ())))))
    ;;    "Defining a class X implies P ⊢τ X")

    ;;   ;; FIXME: don't relies on state
    ;;   ;; (check-not-false
    ;;   ;;  (check-⊢τ #'X)
    ;;   ;;  "Defining a class X implies P ⊢τ X")
    ;;   (check-exn
    ;;    #rx"Unexpected Type.+?in: Y"
    ;;    (thunk (?> #`(prog (class X  []) (new (ow-scheme Y x ())))))
    ;;    "Unknonw type Y if class Y is not defined")
    ;;   (check-exn #rx"Unexpected Type.+?in: τ✘"
    ;;              (thunk (?> #`(class X [] (field x #,τ✘)))))
    ;;   (check-exn #rx"Unexpected Type.+?in: τ✘"
    ;;              (thunk (?> #`(def (x #,τ✘) _))))
    ;;   (check-exn #rx"Unexpected Type.+?in: τ✘"
    ;;              (thunk (?> #`(def (x (_ #,τ✘) #,τ✔) _))))
    ;;   (check-exn #rx"Unexpected Type.+?in: τ✘"
    ;;              (thunk (?> #`(let (x #,τ✘ _) _))))
    ;;   (check-exn #rx"Unexpected Type.+?in: τ✘"
    ;;              (thunk (?> #`(new #,τ✘)))))
    ;; )
   )

  ;; No unbound identifier everywhere an `E` is expected. Desugaring
  ;; ensures that all identifiers are binded with a let

  )
  )

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
           #:with OWNER #'Θ
           #:with TYPE #'T
           #:with CPARAMS #'())
  (pattern (T:id PARAMS:id ...+)
           #:with OWNER #'Θ
           #:with TYPE #'T
           #:with CPARAMS #'(PARAMS ...)))

(define-syntax-class arg
  #:description "argument with its type"
  #:literal-sets [type-lits]
  (pattern (NAME:id : T:type)
           #:attr OWNER #'T.OWNER
           #:attr TYPE  #'T.TYPE
           #:attr CPARAMS #'T.CPARAMS))


;; Utils

;; For
;; #:with OW-SCHEME (type∗>ow-scheme #'ARG.T)
(define type∗>ow-scheme (syntax-parser
  [T:type (make-ow-scheme #'T.TYPE #'T.OWNER #'T.CPARAMS
                          #:srcloc #'T)]))

(define-syntax-rule (stx/this-loc pattern)
  (syntax/loc this-syntax pattern))

;; See, https://github.com/racket/racket/blob/2b567b4488ff92e2bc9c0fbd32bf7e2442cf89dc/pkgs/at-exp-lib/at-exp/lang/reader.rkt#L15
;; (define-values
;;   (surface-read surface-read-syntax surface-get-info)
;;   (make-meta-reader
;;    'surface-lang
;;    "language path"
;;    lang-reader-module-paths
;;    s-reader
;;    TODO...))
