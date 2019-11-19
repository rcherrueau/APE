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
;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects
;;
;; Environments:
;; - Γ is the map (VAR . TYPE) of locally bounded variables
;; - τ is the type of the current class

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
         "definitions.rkt")

(provide ?>)


;; ?> :: stx -> stx
(define-parser ?>
  #:literal-sets [keyword-lits expr-lits]

  ;; ⊢p P ⇒ ?P : t
  ;;
  ;; `P` elaborates to `?P` with type `t`
  ;;
  ;; [prog]
  [(prog ~! CLASS ... E)
   ;; #:and (~do (dbg this-syntax))
   ;; Check no duplicated class names
   #:when (no-name-clash? #'(CLASS ...))
   ;; Check P ⊢d CLASS ⇒ ?CLASS
   #:with [?CLASS ...] (stx-map ?> #'(CLASS ...))
   ;; Check P,[] ⊢e E ⇒ ?E : t
   #:with [?E t] (get-τ (with-Γ #'() (?> #'E)))
   ;; ----------------------------------------------------------------
   ;; ⊢p (prog CLASS ... E) ⇒ (prog ?CLASS ... ?E) : t
   (add-τ this-syntax #'t)]


  ;; P ⊢d CLASS ⇒ ?CLASS
  ;;
  ;; `CLASS` elaborates to `?CLASS` (under `P`)
  ;;
  ;; [defn]
  [(class ~! NAME [CPARAM ...] FIELD/DEF ...)
   ;; #:and (~do (dbg this-syntax))
   #:with [FIELD ...] (filter field? (stx->list #'(FIELD/DEF ...)))
   #:with [DEF ...] (filter def? (stx->list #'(FIELD/DEF ...)))
   ;; Check no duplicated field and def names
   #:when (no-name-clash? #'(FIELD ...))
   #:when (no-name-clash? #'(DEF ...))
   ;; Check P ⊢τ t on field
   #:with [(field ~! F-NAME F-OW-SCHEME) ...] #'(FIELD ...)
   #:when (stx-map ?> #'(F-OW-SCHEME ...))
   ;; Check P,NAME ⊢m DEF ⇒ ?DEF
   #:with [?DEF ...] (with-τ #'NAME (stx-map ?> #'(DEF ...)))
   ;; ----------------------------------------------------------------
   ;;p ⊢d (class NAME FIELD ... DEF ...) ⇒ (class NAME FIELD ... ?DEF ...)
   this-syntax]


  ;; P,τ ⊢m DEF ⇒ ?DEF
  ;;
  ;; `DEF` in `τ` elaborates to `?DEF`
  ;;
  ;; [meth]
  [(def ~! (NAME (ARG-NAME ARG-OW-SCHEME) ... RET-OW-SCHEME) E)
   ;; #:and (~do (dbg this-syntax))
   ;; Get current class type store in τ environment
   #:with τ0 (τ)
   ;; Check P ⊢τ t on args and return type
   #:when (stx-map ?> #'(ARG-OW-SCHEME ... RET-OW-SCHEME))
   ;; Check P,{this: τ0, ARG-NAME: ARG-OW-SCHEME, ...} ⊢e E ⇒ ?E : RET-OW-SCHEME
   #:with [?E t-e] (get-τ
                    (with-Γ #'{(this . (τ0 Θ ()))
                               (ARG-NAME . ARG-OW-SCHEME)
                               ...}
                      (?> #'E)))
   #:when (τ=? #'t-e #'RET-OW-SCHEME #:srcloc #'?E)
   ;; ----------------------------------------------------------------
   ;; P,τ0 ⊢m (def (NAME (ARG-NAME ARG-OW-SCHEME) ... RET-OW-SCHEME) E) ⇒
   ;;           (def (NAME (ARG-NAME ARG-OW-SCHEME) ... RET-OW-SCHEME) ?E)
   this-syntax]


  ;; P,Γ ⊢e E ⇒ ?E : t
  ;;
  ;; `E` elaborates to `?E` with type `t`
  ;;
  ;; [new]
  [(new ~! OW-SCHEME)
   ;; #:and (~do (dbg this-syntax))
   ;; Check P ⊢τ VAR-OW-SCHEME
   #:when (?> #'OW-SCHEME)
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e (new C) ⇒ (new C) : C
   (add-τ this-syntax #'OW-SCHEME)]


  ;; [var]
  ;; Check ID ∈ dom(Γ)
  ;;;; ID is locally bound
  [ID:id #:when (Γ-member? #'ID)
   ;; #:and (~do (dbg this-syntax))
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e ID ⇒ ID : Γ(ID)
   (add-τ this-syntax (Γ-ref #'ID))]
  [ID:id ;; Not locally bound? ⇒ unbound identifier.
   ;; #:and (~do (dbg this-syntax))
   ;;;; This is not supposed to happened thanks to desugaring, but who
   ;;;; knows ...
   (raise-syntax-error #f "unbound identifier" #'ID)]


  ;; [get]
  [(get-field ~! E FNAME)
   ;; #:and (~do (dbg this-syntax))
   ;; Check P,Γ ⊢e E ⇒ ?E : ?t
   #:with [?E t:ow-scheme] (get-τ (?> #'E))
   ;; Check (t . FNAME) ∈ dom(FS)
   ;;;; The field FNAME is defined in the class t
   #:fail-unless (FS-member? #'(t.TYPE . FNAME))
   (format "~a is not a field of ~a"
           (syntax->datum #'FNAME)
           (syntax->datum #'(t.TYPE)))
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e (get-field E FNAME) ⇒
   ;;          (get-field (?E : t) FNAME) : FS(t . FNAME)
   (add-τ this-syntax (FS-ref #'(t.TYPE . FNAME)))]


  ;; [set]
  [(set-field! ~! E FNAME BODY)
   ;; #:and (~do (dbg this-syntax))
   ;; Check P,Γ ⊢e E ⇒ ?E : ?t
   #:with [?E t:ow-scheme] (get-τ (?> #'E))
   ;; Check (t . FNAME) ∈ dom(FS)
   ;;;; The field FNAME is defined in the class t
   #:fail-unless (FS-member? #'(t.TYPE . FNAME))
   (format "~a is not a field of ~a"
           (syntax->datum #'FNAME)
           (syntax->datum #'(t.TYPE)))
   ;; Check P,Γ ⊢e BODY ⇒ ?BODY : FS(t . FNAME)
   ;;;; The BODY has to elaborate to something that fit into the
   ;;;; field.
   #:with [?BODY t-body] (get-τ (?> #'BODY))
   #:with t-field (FS-ref #'(t.TYPE . FNAME))
   #:when (τ=? #'t-body #'t-field)
   ;; ----------------------------------------------------------------
   ;; P,Γ ⊢e (set-field E FNAME BODY) ⇒
   ;;          (set-field (?E : t) FNAME ?BODY) : FS(t . FNAME)
   (add-τ this-syntax #'t-field)]


  ;; [call]
  [(send ~! E DNAME PARAM ...)
   ;; #:and (~do (dbg this-syntax))
   ;; Check P,Γ ⊢e E ⇒ ?E : t
   #:with [?E t] (get-τ (?> #'E))
   ;; Check P,Γ ⊢e PARAM ... ⇒ (?PARAM : t) ...
   #:with [(?PARAM t-param) ...] (stx-map (∘ get-τ ?>) #'(PARAM ...))
   ;; ;; TODO
   ;; ;; Check (t DNAME (t-param ...)) ∈ dom(DS)
   ;; ;;;; The method DNAME with parameters (t-param ...) is defined in
   ;; ;;;; the class t.
   ;; #:with KEY #'(t.TYPE DNAME (t-param ...))
   ;; #:fail-unless (DS-member? #'KEY)
   ;; (format "~a with arguments ~a is not a method of ~a"
   ;;         (syntax->datum #'DNAME)
   ;;         (syntax->datum #'(t-param ...))
   ;;         (syntax->datum #'(t.TYPE)))
   ;; ;; ----------------------------------------------------------------
   ;; ;; P,Γ ⊢e (set-field E FNAME BODY) ⇒
   ;; ;;          (set-field (?E : t) FNAME ?BODY) : FS(t . FNAME)
   ;; ;; (add-τ this-syntax (DS-ref? #'KEY))]
   this-syntax]


  ;; [let]
  [(let ~! (VAR-NAME VAR-OW-SCHEME E) BODY)
   ;; #:and (~do (dbg this-syntax))
   ;; Check P ⊢τ VAR-OW-SCHEME
   #:when (?> #'VAR-OW-SCHEME)
   ;; Check  P,Γ ⊢e E => ?E : VAR-OW-SCHEME
   #:with [?E t] (get-τ (?> #'E))
   #:when (τ=? #'t #'VAR-OW-SCHEME #:srcloc #'?E)
   ;; Check P,Γ{VAR-NAME: VAR-OW-SCHEME} ⊢e BODY => ?BODY : t
   #:with [_ t-body] (get-τ
                      (with-Γ (Γ-set #'(VAR-NAME . VAR-OW-SCHEME))
                        (?> #'BODY)))
   ;; ------------------------------------------------------------------
   ;;   P,Γ ⊢e *let (VAR-NAME VAR-OW-SCHEME E) BODY ⇒
   ;;             *let (VAR-NAME VAR-OW-SCHEME ?E) ?BODY : t
   (add-τ this-syntax #'t-body)]


  ;; P ⊢τ t
  ;;
  ;; `t` exists (in `P`)
  ;;
  ;; [type]
  [OWS:ow-scheme #:when (CS-member? #'OWS.TYPE)
   ;; #:and (~do (dbg this-syntax))
   this-syntax]
  [OWS:ow-scheme ;; Not exists? ⇒ unexpected type.
   ;; #:and (~do (dbg this-syntax))
   (raise-syntax-error #f "Unexpected Type" #'OWS.TYPE)])


;; Environment

;; Store the current class type
(define τ (make-parameter #f))

;; Make `the-τ` a new value for (τ) parameter in the context of STX.
;; : TYPE (-> STX) -> STX
(define (private:with-τ the-τ thunk-E)
  (parameterize ([τ the-τ]) (thunk-E)))

(define-syntax-parser with-τ
  ;; Automatically create the `thunk` around E expression
  [(_ THE-τ E:expr ...) #'(private:with-τ THE-τ (thunk E ...))])

;; ~~~~~~~~~~~~~~~~~~~~
;; Manage local binding

;;
;; Map Γ of local bindings.
;;
;; Keys of the map are `identifier?`, and keys are compared with a
;; sort of `bound-identifier=?` (i.e., doesn't take scope into
;; account).
;;
;; : -> (Id-Hash (VAR . TYPE))
(define-custom-hash-types id-hash
  #:key? identifier?
  bound-id=?)
(define Γ (make-parameter (make-immutable-id-hash)))

;; Is VAR bounded in Γ?
;; : VAR -> Boolean
(define (Γ-member? VAR)
  (dict-has-key? (Γ) VAR))

;; Set TYPE of VAR in Γ
;; > (Γ-set #'(VAR . TYPE))
;; : #'(VAR . TYPE) -> Boolean
(define (Γ-set VAR.TYPE)
  (let* ([VAR-&-TYPE (syntax-e VAR.TYPE)]
         [VAR (car VAR-&-TYPE)]
         [TYPE (cdr VAR-&-TYPE)])
    (dict-set (Γ) VAR TYPE)))

;; Returns the TYPE of VAR in Γ
;; : VAR -> TYPE
(define (Γ-ref VAR)
  (dict-ref (Γ) VAR))

;; Make `the-Γ` a new value for (Γ) parameter by mapping it into a
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


;; Exceptions

;;;; Name clash
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
          (string-append srcloc-msg ": " id ": " err-msg
                         previous-ID-msg)
          (current-continuation-marks)
          (list (syntax-taint ID)))))

;;;; Type mismatch
;; (struct exn:type-mismatch exn:fail:syntax ()
;;   #:extra-constructor-name make-exn:type-mismatch
;;   #:transparent)
;;
;; (define (raise-type-mismatch ID IDs)
;;   (define srcloc-msg (srcloc->string (build-source-location ID)))
;;   (define id (format "~s" (syntax->datum ID)))
;;   (define err-msg "multiple declaration")
;;   (define previous-ID (findf (curry bound-identifier=? ID) IDs))
;;   (define previous-ID-msg (format "~n  previously seen at line ~a:~a"
;;                                   (syntax-line previous-ID)
;;                                   (syntax-column previous-ID)))
;;
;;   (raise (make-exn:name-clash
;;           (string-append srcloc-msg ": " id ": " err-msg
;;                          previous-ID-msg)
;;           (current-continuation-marks)
;;           (list (syntax-taint ID)))))


;; Utils
(define (get-τ stx)
  #`(#,stx #,(type-prop stx)))

(define (τ=? ACTUAL-TYPE EXPECTED-TYPE #:srcloc [ctx (current-syntax-context)])
  (syntax-parse #`(#,ACTUAL-TYPE #,EXPECTED-TYPE)
    [(ow1:ow-scheme ow2:ow-scheme)
     (cond
       [(bound-id=? #'ow1.TYPE #'ow2.TYPE) #t]
       [else
        (define $err "type mismatch;~n  expected: ~s~n  given: ~s")
        (define given (syntax->datum #'ow1.TYPE))
        (define expected (syntax->datum #'ow2.TYPE))
        (raise-syntax-error #f (format $err expected given) ctx)])]
    [(~or (#f _) (_ #f))
     (log-fatal (string-append "; "
                               "~a from ~a is not a proper pair of "
                               "ownership types because one of theme "
                               "comes from an untyped term. "
                               "However, we make the type checking to "
                               "work for prototyping reasons. ")
                (syntax->datum this-syntax)
                (syntax->datum ctx)) #t]
    [_ (error 'т=? "~a from ~a is not a proper pair of ownership types"
              (syntax->datum this-syntax)
              (syntax->datum ctx))]))

(define (add-τ stx TYPE)
  (type-prop stx TYPE))

;; Ensures no name clash
(define (no-name-clash? stxs)
  (define get-name
    (syntax-parser
      #:literal-sets [keyword-lits]
      [(class name _ ...) #'name]
      [(field name _ ...) #'name]
      [(def (name _ ...) _) #'name]))

  (define names (map get-name (stx->list stxs)))
  (define $err "multiple ~s declarations")

  (cond
    ;; A duplicate name exists
    [(check-duplicate-identifier names)
     => (λ (name) (raise-name-clash name names))]
    ;; Everything is fine
    [else #t]))

;; Returns (type-prop E) if (type-prop E) has type TYPE, raise a
;; syntax error otherwise
(define (check-⊢e E TYPE)
  (define expected-type (syntax->datum TYPE))
  (define e-type (syntax->datum (type-prop E)))

  (cond
    [(eq? expected-type e-type) (type-prop E)]
    [else
     (define $err "type mismatch;~n  expected: ~s~n  given: ~s")
     (raise-syntax-error #f (format $err expected-type e-type) E)]))

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
  (require rackunit
           rackunit/text-ui)

  ;; Default EXPR/TYPE
  (define τ✔-name #'τ✔)
  (define τ✔ #`(#,τ✔-name type ()))
  (define EXPR:τ✔ #`(new #,τ✔))

  ;; Unknown EXPR/TYPE
  (define τ✘-name #'τ✘)
  (define τ✘ #`(#,τ✘-name type ()))
  (define EXPR:τ✘ #`(new #,τ✘))

  (define test:Γ (make-immutable-id-hash
                  (list (cons #'foo τ✔)
                        (cons #'bar τ✔))))

  (run-tests
   (test-suite
    "Tests for basic checks transformation (?>)"

    ;; ----------------------------------------------------------------
    ;; Duplicated names
    (test-case "Name clash"

      (check-exn
       exn:name-clash?
       (thunk (?> #'(prog (class X []) (class X []) _)))
       "class name is unique")

      (check-exn
       exn:name-clash?
       (thunk (?> #'(class X [] (field x _) (field x _))))
       "field name is unique in a class")

      (check-not-exn
       (thunk (?> #`(prog (class X [] (field x #,τ✔))
                          (class Y [] (field x #,τ✔)))))
       "field name is only unique within a class")

      (check-exn
       exn:name-clash?
       (thunk (?> #'(class X [] (def (x _) _) (def (x _) _))))
       "def name is unique in a class")

      (check-not-exn
       (thunk (?> #`(prog (class X [] (def (x #,τ✔) #,EXPR:τ✔))
                          (class Y [] (def (x #,τ✔) #,EXPR:τ✔)))))
       "def name is only unique within a class")
      )

    ;; ----------------------------------------------------------------
    ;; Local bindings
    (test-case "Local bindings"

      ;; with syntax list
      (with-Γ #'((foo . Integer) (bar . String))
        ;; member
        (check-true  (Γ-member? #'foo))
        (check-false (Γ-member? #'baz))

        ;; set
        (check-true  (with-Γ (Γ-set #'(baz . _)) (Γ-member? #'baz)))
        (check-false (with-Γ (Γ-set #'(baz . _)) (Γ-member? #'xyzzy)))

        ;; ref
        (check-stx=? (Γ-ref #'foo) #'Integer)
        (check-stx=? (Γ-ref #'bar) #'String))

      ;; With hash-table
      (with-Γ (make-immutable-id-hash (list (cons #'foo #'Integer)
                                            (cons #'bar #'String)))
        ;; member
        (check-true  (Γ-member? #'foo))
        (check-false (Γ-member? #'baz))

        ;; set
        (check-true  (with-Γ (Γ-set #'(baz . _)) (Γ-member? #'baz)))
        (check-false (with-Γ (Γ-set #'(baz . _)) (Γ-member? #'xyzzy)))

        ;; ref
        (check-stx=? (Γ-ref #'foo) #'Integer)
        (check-stx=? (Γ-ref #'bar) #'String)))

    ;; ----------------------------------------------------------------
    ;; Unbound identifier
    (test-case "Bound & unbound identifier"
      (check-not-exn
       (thunk (with-Γ #`((id . _)) (?> #'id)))
       "id is bounded if it exists in Γ")
      (check-exn
       #rx"unbound identifier.+?in: id"
       (thunk (?> #'id))
       "id is unbound if it does not exist in Γ")

      ;; def
      (check-not-exn
       (thunk (?> #`(def (x (id #,τ✔) #,τ✔) id)))
       "id is bounded if it is a parameter of the def")
      (check-not-exn
       (thunk (?> #`(def (x (id #,τ✔) #,τ✔) this)))
       "this is bounded in a def")
      (check-exn
       #rx"unbound identifier.+?in: id"
       (thunk (?> #`(def (x #,τ✔) id)))
       "id is unbound if it is not a parameter of the def")

      ;; let
      (check-not-exn
       (thunk (?> #`(let (id #,τ✔ #,EXPR:τ✔) id)))
       "let binder bound the id")
      (check-exn
       #rx"unbound identifier.+?in: id"
       (thunk (?> #`(let (x #,τ✔ id) x)))
       "id is unbound in the binder of a let if it does not exist in Γ")
      (check-exn
       #rx"unbound identifier.+?in: id"
       (thunk (?> #`(let (x #,τ✔ #,EXPR:τ✔) id)))
       "id is unbound in the body of a let if it does not exist in Γ")

      ;; FIXME: Right now, unbound ids in class are forgotten. Rather, an
      ;; exception should be raised.
      ;; (check-exn
      ;;           #rx"unbound identifier.+?in: id"
      ;;           (thunk (?> #'(class X []  id))))
      (check-exn #rx"unbound identifier.+?in: id"
                (thunk (?> #`(get-field id _))))

      (check-exn #rx"unbound identifier.+?in: id"
                 (thunk (?> #`(set-field! id _ _)))
                 "id is bound in the receiver object of a set-field!")

      (check-exn #rx"unbound identifier.+?in: id"
                 (thunk (?> #`(set-field! #,EXPR:τ✔ _ id)))
                 "id is bound in the body of a set-field!")

      (check-exn #rx"unbound identifier.+?in: id"
                 (thunk (?> #`(send id _)))
                 "id is bound in the receiver object of a send")

      (check-exn #rx"unbound identifier.+?in: id"
                 (thunk (?> #`(send #,EXPR:τ✔ _ id)))
                 "id is bound in the argument of a send")
      )


    ;; ----------------------------------------------------------------
    ;; Unknown Type
    (test-case "Known & Unknown Types"
      (check-not-exn
       (thunk (?> #`(prog (class X  []) (new (ow-scheme X x ())))))
       "Defining a class X implies P ⊢τ X")

      ;; FIXME: don't relies on state
      ;; (check-not-false
      ;;  (check-⊢τ #'X)
      ;;  "Defining a class X implies P ⊢τ X")
      (check-exn
       #rx"Unexpected Type.+?in: Y"
       (thunk (?> #`(prog (class X  []) (new (ow-scheme Y x ())))))
       "Unknonw type Y if class Y is not defined")
      (check-exn #rx"Unexpected Type.+?in: τ✘"
                 (thunk (?> #`(class X [] (field x #,τ✘)))))
      (check-exn #rx"Unexpected Type.+?in: τ✘"
                 (thunk (?> #`(def (x #,τ✘) _))))
      (check-exn #rx"Unexpected Type.+?in: τ✘"
                 (thunk (?> #`(def (x (_ #,τ✘) #,τ✔) _))))
      (check-exn #rx"Unexpected Type.+?in: τ✘"
                 (thunk (?> #`(let (x #,τ✘ _) _))))
      (check-exn #rx"Unexpected Type.+?in: τ✘"
                 (thunk (?> #`(new #,τ✘))))))
   )

  ;; No unbound identifier everywhere an `E` is expected. Desugaring
  ;; ensures that all identifiers are binded with a let

  )
