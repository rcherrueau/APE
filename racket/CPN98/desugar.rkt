#lang racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.
;;
;; Desugaring syntax phase (∗>)
;; - Introduce missing Context parameters (CPARAM).
;; - Transform let with multiple binding into nested lets of one
;;   binding.
;; - Expand "short field" access to "canonical field" access:
;;   (do-something field) ∗> (do-something (get-field *this* field)).
;; - Expand types to ownership schemes.
;;
;; Environment:
;; - Γ is the set of locally bound variables.  Used to track free
;;   variables, that are judged as "short fields".
;;
;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects

(require (for-syntax racket/base)
         racket/match
         racket/syntax
         syntax/parse
         syntax/srcloc
         syntax/stx
         "utils.rkt"
         "definitions.rkt"
         (prefix-in env: (submod "env.rkt" desugar)))

(module+ test (require rackunit))

(provide ∗>)



;; Phase ∗>

(define-phase (∗> stx)

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Env

  ;; Set of local bindings
  ;;
  ;; (: Γ (Setof Identifier))
  (Γ #:init   '()
     #:mk     env:make-Γ
     #:apply? (env:Γ-member? env:Γ-add))

  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Parser
  (∗p> stx))


;; Program rules

;; A prog is a list of CLASS and one expression E
(define-rules ∗p>
  ;; Note: The `~!` eliminate backtracking. Hence, if the next
  ;; `fail-when` failed, it will not backtrack to try other cases.
  ;;
  ;; TODO: Add a ~commit to see if it corrects backtracking during an
  ;; error.
  [(prog ~! CLASS:expr ... E:expr)
   #:with [*CLASS ...] (stx-map ∗c> #'(CLASS ...))
   #:with *E           (∗e> #'E)
   (stx/surface (prog *CLASS ... *E) this-syntax)])


;; Class rules

;; A class is a NAME, an optional list of context parameters
;; CPARAM, and a list of fields and definitions.
(define-rules ∗c>
  [(class NAME:id [CPARAM:id ...] ~! FIELD/DEF:expr ...)
   #:with [*FIELD/DEF ...] (stx-map ∗f/d> #'(FIELD/DEF ...))
   (stx/surface (class NAME [CPARAM ...] *FIELD/DEF ...) this-syntax)]
  ;; Transforms a `class` without `CPARAM ...` into a `class` with.
  [(class NAME:id ~! FIELD/DEF ...)
   (∗c> (stx/surface (class NAME [] FIELD/DEF ...) this-syntax))])

(module+ test
  (define-test-suite ∗c>-parse
    (check-stx=? (∗c> #'(class foo)) #'(class foo ())
                 #:msg "`class` with no CPARAM implies empty CPARAMS")
    (check-stx=? (∗c> #'(class foo (field [bar : o/t])))
                 #'(class foo () (field bar (t o ())))
                 #:msg "`class` with no CPARAM implies empty CPARAMS")
    (check-ill-parsed
      (∗c> #'(class foo (field [bar : o/t]) bar))
      #:msg (string-append
             "A class is a list of Fields and Defs "
             "(Arbitrary expression such as `bar` is not allowed)"))))


;; Fields and defs rules

(define-rules ∗f/d>
  ;; A field declares one argument ARG (i.e., no initialization).
  ;;
  ;; (field ARG)
  ;; ∗>  (field NAME OW-SCHEME)
  ;; with
  ;;     OW-SCHEME := (ow-scheme TYPE OWNER CPARAMS)
  [(field ~! ARG:arg)
   (stx/surface (field ARG.NAME ARG.OW-SCHEME) this-syntax)]

  ;; A def (i.e., method) is a NAME, a list of arguments ARG, a
  ;; return type RET and the BODY of the def. The def binds ARG in
  ;; the BODY. The Γ, during transformation of BODY, contains the
  ;; local binding (i.e., `ARG.NAME ...`) of the def, plus `this`.
  ;;
  ;; (def (NAME ARG ... → RET) BODY)
  ;; ∗>  (def (NAME (A-NAME A-OW-SCHEME) ... RET-OW-SCHEME) *BODY)
  ;; with
  ;;     A-OW-SCHEME := (A-TYPE A-OWNER A-CPARAMS)
  [(def ~! (NAME:id ARG:arg ... . RET:rtype) BODY:expr ...+)
   #:with [A-NAME ...]      #'(ARG.NAME ...)
   #:with [A-OW-SCHEME ...] #'(ARG.OW-SCHEME ...)
   #:with RET-OW-SCHEME     #'RET.OW-SCHEME
   #:with [*BODY ...]       (with-Γ #'(this A-NAME ...) (stx-map ∗e> #'(BODY ...)))
   (stx/surface (def (NAME (~@ (A-NAME A-OW-SCHEME)) ... RET-OW-SCHEME) *BODY ...)
                this-syntax)])

(module+ test
  (define-test-suite ∗f/d>-parse
    ;; field
    (check-stx=? (∗f/d> #'(field [name : o/t]))
                 #'(field name (t o ())))

    ;; def
    (check-stx=? (∗f/d> #'(def (name -> o/t) ???))
                 #'(def (name (t o ())) ???)
                 #:msg "`def` with no argument is valid")
    (check-stx=? (∗f/d> #'(def (name [foo : o/t] [bar : o/t] -> o/t) ???))
                 #'(def (name (foo (t o ())) (bar (t o ())) (t o ())) ???)
                 #:msg "`def` with multiple arguments is valid")
    (check-stx=? (∗f/d> #'(def (name [foo : o/t] [bar : o/t] -> o/t) foo ???))
                 #'(def (name (foo (t o ())) (bar (t o ())) (t o ())) foo ???)
                 #:msg "`def` with multiple BODY exprs are kept in order")
    (check-ill-parsed (∗f/d> #'(def (name [foo : o/t]) ???))
                      #:msg "`def` with no return type is not valid")
    (check-ill-parsed (∗f/d> #'(def (name [foo : o/t])))
                      #:msg "`def` always provide a BODY")

    ;; arg bind
    (check-stx=? (∗f/d> #'(def (name [foo : o/t] -> o/t) bar))
                 #'(def (name (foo (t o ())) (t o ())) (get-field this bar))
                 #:msg "`def` expands its BODY and binds `this`")
    (check-stx=? (∗f/d> #'(def (name [foo : o/t] -> o/t) foo))
                 #'(def (name (foo (t o ())) (t o ())) foo)
                 #:msg "`def` binds `foo` identifier")
    (check-stx=? (∗f/d> #'(def (name [foo : o/t] [bar : o/t] -> o/t) foo bar))
                 #'(def (name (foo (t o ())) (bar (t o ())) (t o ())) foo bar)
                 #:msg "`def` args are bind for all expressions of the BODY")))


;; Expression rules

(define-rules ∗e>
  ;; A let binds a variables VAR with a type T to an expression E in
  ;; a BODY. During the transformation of BODY, Γ is extended with
  ;; the newly bound variable VAR.
  ;;
  ;; (let ([VAR : T E] ...) BODY)
  ;; ∗>  (let (VAR OW-SCHEME *E) (let... (...) *BODY)
  ;; with
  ;;     OW-SCHEME := (TYPE OWNER CPARAMS)
  [(let ([VAR:id : T:type E:expr]) ~! BODY:expr ...+)
   #:with *E (∗e> #'E)
   #:with [*BODY ...]  (with-Γ (Γ-add #'VAR) (stx-map ∗e> #'(BODY ...)))
   (stx/surface (let (VAR T.OW-SCHEME *E) *BODY ...) this-syntax)]
  ;; Lowering a `let` with multiple binding into multiple nested
  ;; `let`s with one unique binding (such as the previous let)
  [(let ~! ([~and HD (VAR:id : T:type E:expr)] TL ...) BODY:expr ...+)
   (∗e> (stx/surface (let (HD) (let (TL ...) BODY ...)) this-syntax))]

  ;; A new takes the class type C-TYPE of the class to instantiate
  ;; (i.e., no constructor).
  ;;
  ;; (new C-TYPE)
  ;; ∗>  (new (TYPE OWNER CPARAMS))
  [(new C-TYPE:type)
   #:cut
   (stx/surface (new C-TYPE.OW-SCHEME) this-syntax)]

  ;; A get-field takes an expression E that should reduce to an
  ;; object and the name of the field FNAME to get on that object.
  ;;
  ;; (get-field E FNAME)
  ;; ∗>  (get-field *E FNAME)
  [(get-field ~! E:expr FNAME:id)
   #:with *E (∗e> #'E)
   (stx/surface (get-field *E FNAME) this-syntax)]

  ;; A set-field! takes an expression E that should reduce to an
  ;; object, the name of the field FNAME to change the value of, and
  ;; the BODY of the new value.
  ;;
  ;; (set-field! E FNAME BODY)
  ;; ∗>  (set-field! *E FNAME *BODY)
  [(set-field! ~! E:expr FNAME:id BODY:expr)
   #:with *E    (∗e> #'E)
   #:with *BODY (∗e> #'BODY)
   (stx/surface (set-field! *E FNAME *BODY) this-syntax)]

  ;; A send takes an expression E that should reduce to an object,
  ;; the name of the def DNAME to call on that object, and a list of
  ;; expressions `E-ARG ...` to pass as arguments to the def.
  ;;
  ;; (send E DNAME E-ARG ...)
  ;; ∗>  (send *E DNAME *E-ARG ...)
  [(send ~! E:expr DNAME:id E-ARG:expr ...)
   #:with *E           (∗e> #'E)
   #:with [*E-ARG ...] (stx-map ∗e> #'(E-ARG ...))
   (stx/surface (send *E DNAME *E-ARG ...) this-syntax)]

  ;; An identifier is either:
  ;;
  ;;;; The debug placeholder ???
  [??? this-syntax]
  ;;;; A bound identifier (from a def or let)
  [ID:id #:when (Γ-member? #'ID)
    this-syntax]
  ;;;; A free identifier in a class (i.e., `this' is bound).  In that
  ;;;; case, it presumably refers to a field of the current class: A
  ;;;; sort of shortcut for (get-field this id) -- i.e., `id` instead
  ;;;; of `this.id` in Java world. E.g.,
  ;;;;
  ;;;; 1 (class C
  ;;;; 2   (field [id : A])
  ;;;; 3   (def (get-id → A) id))
  ;;;;
  ;;;; With line 3, a shortcut for
  ;;;; > (def (get-id → A) (get-field this id))
  ;;;;
  ;;;; We remove it, so the desugared syntax contains no free
  ;;;; identifier.
  ;;;; ID ∗> (get-field this ID)
  [ID:id #:when (Γ-member? #'this)
    (∗e> (stx/surface (get-field this ID) this-syntax))]
  ;;;; Unbound identifier
  [ID:id
   (raise (mk-exn:unbound-id #'ID))])

(module+ test
  (define-test-suite ∗e>-parse
    ;; Bound `this` and `foo` in the following
    (with-Γ #'(this foo)
      ;;Identifier
      (check-stx=? (∗e> #'foo) #'foo
                   #:msg "Bound identifier is left as it")
      (check-stx=? (∗e> #'bar) #'(get-field this bar)
                   #:msg "Free identifier is expended with `get-field` under `this'")
      (check-stx=? (∗e> #'???) #'???
                   #:msg "The debug placeholder `???` is a valid expr")
      (check-exn exn:fail:syntax:unbound?
                 (λ () (with-Γ #'() (∗e> #'bar)))
                 "Free identifier raised unbound error")

      ;; let
      (check-stx=? (∗e> #'(let ([foo : o/t ???]) ???))
                   #'(let (foo (t o ()) ???) ???))
      (check-stx=? (∗e> #'(let ([foo : o/t (c) ???]) ???))
                   #'(let (foo (t o (c)) ???) ???))
      (check-stx=? (∗e> #'(let ([foo : t  ???]) ???))
                   #'(let (foo (t world ()) ???) ???))
      (check-stx=? (∗e> #'(let ([foo : t (c) ???]) ???))
                   #'(let (foo (t world (c)) ???) ???))
      (check-stx=? (∗e> #'(let ([foo : o/t ???]) bar))
                   #'(let (foo (t o ()) ???) (get-field this bar))
                   #:msg "`let` expands its BODY")
      (check-stx=? (∗e> #'(let ([bar : o/t ???]) bar))
                   #'(let (bar (t o ()) ???) bar)
                   #:msg "`let` binds `bar` identifier")
      (check-stx=?
         (∗e> #'(let ([foo : o/t ???]) (let ([bar : o/t ???]) ???)))
         #'(let (foo (t o ()) ???) (let (bar (t o ()) ???) ???))
         #:msg "Nested `let`s")
      (check-stx=?
         (∗e> #'(let ([foo : o/t ???][bar : o/t ???]) ???))
         #'(let (foo (t o ()) ???) (let (bar (t o ()) ???) ???))
         #:msg "`let` with multiple bindings is transformed into nested `let`s")
      (check-stx=?
         (∗e> #'(let ([foo : o/t ???]) foo ???))
         #'(let (foo (t o ()) ???) foo ???)
         #:msg "`let' with multiple BODY exprs are kept in order")
      (check-stx=?
         (∗e> #'(let ([foo : o/t ???] [bar : o/t ???]) foo bar))
         #'(let (foo (t o ()) ???) (let (bar (t o ()) ???) foo bar))
         #:msg "`let' binds arguments for all exprs of the BODY")
      (check-ill-parsed (∗e> #'(let ([]) ???)))
      (check-ill-parsed (∗e> #'(let ([foo : t c ???]) ???))
         #:msg "context parameters `c` should be surrounded by parentheses")
      (check-ill-parsed (∗e> #'(let ([foo : o/t ???])))
         #:msg "`let` always provide a BODY")
      (check-ill-parsed  ;;; Regression test from hotedge 2020
         (∗e> #'(let ([foo : [o/t c] ???][bar : [o/t c] ???]) ???)))

      ;; new
      (check-stx=? (∗e> #'(new t))       #'(new (t world ())))
      (check-stx=? (∗e> #'(new o/t))     #'(new (t o ())))
      (check-stx=? (∗e> #'(new o/t (c))) #'(new (t o (c))))
      (check-stx=? (∗e> #'(new o/t (c))) #'(new (t o (c)))
                   #:msg "`new` accepts no surrounded o/t (c) syntax")
      (check-ill-parsed (∗e> #'(new)))

      ;; get-field
      (check-stx=? (∗e> #'(get-field object field))
                   #'(get-field (get-field this object) field)
                   #:msg "`get-field` expands its BODY")
      (check-ill-parsed (∗e> #'(get-field () f)))

      ;; set-field!
      (check-stx=? (∗e> #'(set-field! object field foo))
                   #'(set-field! (get-field this object) field foo)
                   #:msg "`set-field!` expands its object")
      (check-stx=? (∗e> #'(set-field! foo field body))
                   #'(set-field! foo field (get-field this body))
                   #:msg "`set-field!` expands its BODY")
      (check-ill-parsed (∗e> #'(set-field! () foo bar)))
      (check-ill-parsed (∗e> #'(set-field! foo bar ())))

      ;; send
      (check-stx=? (∗e> #'(send object def foo))
                   #'(send (get-field this object) def foo)
                   #:msg "`send` expands its object")
      (check-stx=? (∗e> #'(send foo def arg1 arg2))
                   #'(send foo def (get-field this arg1) (get-field this arg2))
                   #:msg "`send` expands its ARG ...")
      (check-stx=? (∗e> #'(send foo def))
                   #'(send foo def)
                   #:msg "`send` without ARG ...")
      (check-ill-parsed (∗e> #'(send () def arg)))
      (check-ill-parsed (∗e> #'(send foo def ()))))))


;; Syntax for type

(define-splicing-syntax-class type
  #:description (string-append
                 "an ownership type"
                 "\n  An ownership type is one of the form:"
                 "\n  - owner/type"
                 "\n  - owner/type{param ...}"
                 "\n  - type -- owner is implictly world"
                 "\n  - type{param ...} -- owner is implicitly world.")
  #:attributes [OW-SCHEME]
  ;; Note: Do not commit to enable backtrack in `let`
  ;; #:commit
  ;;
  ;; Note: Also, order of patterns matters!
  ;;
  ;; owner/type {param ...}
  (pattern (~seq O/T:id {PARAM:id ...+}) #:when (is-stx-owner/type? #'O/T)
           #:with [OWNER . TYPE] (owner/type->OWNER.TYPE #'O/T)
           #:with CPARAMS #'(PARAM ...)
           #:with SURFACE-STX (syntax/loc #'O/T (O/T CPARAMS))
           #:attr OW-SCHEME (stx/surface (TYPE OWNER CPARAMS) #'SURFACE-STX))
  ;; owner/type -- there are no context parameters
  (pattern (~seq O/T:id) #:when (is-stx-owner/type? #'O/T)
           #:with [OWNER . TYPE] (owner/type->OWNER.TYPE #'O/T)
           #:with CPARAMS #'()
           #:attr OW-SCHEME (stx/surface (TYPE OWNER CPARAMS) #'O/T))
  ;; type {param ...} -- owner is implicitly world
  (pattern (~seq TYPE:id {PARAM:id ...+})
           #:with OWNER #'world
           #:with CPARAMS #'(PARAM ...)
           #:with SURFACE-STX (syntax/loc #'TYPE (TYPE CPARAMS))
           #:attr OW-SCHEME (stx/surface (TYPE OWNER CPARAMS) #'SURFACE-STX))
  ;; type -- owner is implicitly world and there are no context parameters
  (pattern (~seq TYPE:id)
           #:with OWNER #'world
           #:with CPARAMS #'()
           #:attr OW-SCHEME (stx/surface (TYPE OWNER CPARAMS) #'TYPE)))

(module+ test
  (define-test-suite type-parse
    ;; The weird `_' in the test is because the `type' is a *splicing*
    ;; syntax pattern. Therefore it is only allowed to appear as a
    ;; /head pattern/ and not /single term pattern/. See,
    ;; https://docs.racket-lang.org/syntax/stxparse-patterns.html?q=splicing-syntax#%28tech._single._term._pattern%29
    (test-pattern #'(_ owner/type) (_ t:type)
      (check-stx=? #'t.OW-SCHEME #'(type owner ())))

    (test-pattern #'(_ owner/type/) (_ t:type)
      (check-stx=? #'t.OW-SCHEME #'(type/ owner ())))

    (test-pattern #'(_ owner/type {c1 c2}) (_ t:type)
      (check-stx=? #'t.OW-SCHEME #'(type owner (c1 c2))))

    (test-pattern #'(_ type) (_ t:type)
      (check-stx=? #'t.OW-SCHEME #'(type world ())))

    (test-pattern #'(_ type{c1 c2}) (_ t:type)
      (check-stx=? #'t.OW-SCHEME #'(type world (c1 c2))))

    (check-ill-parsed
     (syntax-parse #'(_ (owner/type))    [(_ t:type) #t])
     #:msg (string-append "A type is a *splicing* head pattern "
                          "and cannot be used as a single term pattern. "
                          "Therefore it cannot be surrounded by parentheses."))
    (check-ill-parsed
     (syntax-parse #'(_ (owner/type {c1 c2}))    [(_ t:type) #t])
     #:msg (string-append "A type is a *splicing* head pattern "
                          "and cannot be used as a single term pattern. "
                          "Therefore it cannot be surrounded by parentheses."))))


;; Syntax for return type (of a def)

(define-syntax-class rtype
  #:description "a return type"
  #:literal-sets [type-lits]
  #:commit
  (pattern (-> T:type) #:attr OW-SCHEME #'T.OW-SCHEME)
  (pattern (→  T:type) #:attr OW-SCHEME #'T.OW-SCHEME))

(module+ test
  (define-test-suite rtype-parse
    (for ([~> (in-list (list #'→ #'->))])
      (test-case (format "Arrow is ~a" (stx->string ~> #:newline? #f))
        (test-pattern #`(#,~> owner/type) r:rtype
          (check-stx=? #'r.OW-SCHEME #'(type owner ())))
        (test-pattern #`(#,~> owner/type {c}) r:rtype
          (check-stx=? #'r.OW-SCHEME #'(type owner (c))))
        (test-pattern #`(#,~> owner/type {c1 c2}) r:rtype
          (check-stx=? #'r.OW-SCHEME #'(type owner (c1 c2))))
        (test-pattern #`(#,~> owner/type/ {c1 c2}) r:rtype
          (check-stx=? #'r.OW-SCHEME #'(type/ owner (c1 c2))))

        (test-pattern #`(#,~> type) r:rtype
          (check-stx=? #'r.OW-SCHEME #'(type world ())))
        (test-pattern #`(#,~> type (c1 c2)) r:rtype
          (check-stx=? #'r.OW-SCHEME #'(type world (c1 c2))))

        (check-ill-parsed
         (syntax-parse #`(#,~> o/t c)          [_:rtype #t])
         #:msg (string-append "Context param `c` has to be surrounded by "
                              "parentheses (or mustaches): `o/t {c}`"))
        (check-ill-parsed
         (syntax-parse #`(#,~> owner/type {})  [_:rtype #t])
         #:msg (string-append "Surrounded parentheses of context params"
                              "have to be omitted if there are empty"))
        (check-ill-parsed
         (syntax-parse #`(#,~> (o/t {c}))      [_:rtype #t])
         #:msg (string-append "A type is a *splicing* syntax therefore "
                              "it requires no surrounding parentheses"))))))


;; Syntax for arguments (of a def, let)

(define-syntax-class arg
  #:description "an argument with its type"
  #:literal-sets [type-lits]
  #:commit
  (pattern (NAME:id : T:type)
           #:attr OW-SCHEME #'T.OW-SCHEME))

(module+ test
  (define-test-suite arg-parse
    (test-pattern #'(name : owner/type) a:arg
      (check-stx=? #'a.OW-SCHEME #'(type owner ())))
    #;(test-pattern #'(name : (owner/type {c1 c2})) a:arg
      (check-stx=? #'a.OW-SCHEME #'(type owner (c1 c2))))
    (test-pattern #'(name : owner/type {c1 c2}) a:arg
      (check-stx=? #'a.OW-SCHEME #'(type owner (c1 c2)))
      #:msg "`arg` accepts no surrounded o/t(c) syntax")

    (test-pattern #'(name : type) a:arg
      (check-stx=? #'a.OW-SCHEME #'(type world ())))
    (test-pattern #'(name : type (c1 c2)) a:arg
      (check-stx=? #'a.OW-SCHEME #'(type world (c1 c2))))

    (check-ill-parsed
     (syntax-parse #'(name {owner/type})            [_:arg #t]))))


;; Exceptions

;; (: mk-exn:unbound-id ((Identifier) ((U Syntax #f)) .->*. exn:fail:syntax:unbound))
(define (mk-exn:unbound-id ID [context #f])
  (define CTX (or context (current-syntax-context)))
  ;; (log-sclang-debug "Desugared syntax is ~.s" CTX)
  (define srcloc-msg (srcloc->string (build-source-location CTX)))
  (define id (format "~s" (extract-exp-name ID)))
  (define err-msg "unbound identifier")
  (define unbound-msg (format "~n  in: ~.s" (syntax->datum CTX)))

  (make-exn:fail:syntax:unbound
   (string-append srcloc-msg ": " id ": " err-msg unbound-msg)
   (current-continuation-marks)
   (list (syntax-taint CTX))))


;; Utils

;; Regexp to match owner/type syntax
;;
;; (: rx-owner/type Regexp)
(define rx-owner/type #rx"([^/]+)/(.+)")

;; Returns #t if a syntax object is of the form #'owner/type (i.e.,
;; with the `/` separator), or #f otherwise.
;;
;; (: is-stx-owner/type? (Identifier -> Boolean))
(define (is-stx-owner/type? stx)
  (and (regexp-match rx-owner/type (symbol->string (syntax-e stx)))
       #t))

;; Split a #'owner/type syntax object on the first `/` and returns a
;; syntax pair #'(owner . type).
;;
;; (: owner/type->OWNER.TYPE
;;    (Syntax -> (Syntaxof (Pairof Identifier Identifier))))
(define (owner/type->OWNER.TYPE stx)
  ;; Get owner and type from `stx`
  (match-define (list _ owner type)
    (regexp-match rx-owner/type (symbol->string (syntax-e stx))))

  ;; Compute the new syntax
  (with-syntax ([OWNER (format-id stx "~a" owner #:source stx)]
                [TYPE  (format-id stx "~a" type #:source stx)])
    #'(OWNER . TYPE)))

(module+ test
  (define-test-suite owner/type-stx-split
    (check-true  (is-stx-owner/type? #'owner/type))
    (check-true  (is-stx-owner/type? #'owner/ty/pe))
    (check-true  (is-stx-owner/type? #'owner/t/y/p/e/))
    (check-false (is-stx-owner/type? #'ownertype))
    (check-false (is-stx-owner/type? #'ownertype/))

    (check-stx=? (owner/type->OWNER.TYPE #'owner/type)
                 #'(owner . type))
    (check-stx=? (owner/type->OWNER.TYPE #'owner/ty/pe)
                 #'(owner . ty/pe))
    (check-stx=? (owner/type->OWNER.TYPE #'owner/t/y/p/e/)
                 #'(owner . t/y/p/e/))))


;; Tests

(module+ test
  (require rackunit/text-ui
           (prefix-in env: (submod "env.rkt" desugar test)))
  (provide desugar-tests)

  (define desugar-tests
    (test-suite "Desugaring phase"
     ;; Check env
     env:Γ-tests
     ;; Check syntax class
     owner/type-stx-split
     type-parse
     rtype-parse
     arg-parse
     ;; Check phase rules
     ∗e>-parse
     ∗f/d>-parse
     ∗c>-parse))

  (run-tests desugar-tests)
  )
