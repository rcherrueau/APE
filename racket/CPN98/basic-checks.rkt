#lang racket/base

;; Implements checks of FKF98
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

(require racket/function
         racket/list
         racket/match
         syntax/parse
         syntax/stx
         "utils.rkt"
         "definitions.rkt")

(provide ?>)

;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects


;; Checking transformation (?>)
;;
;; - Checks no duplicate class name, field, def
;; - Type checks the program (for simple type -- "simple" as in simply
;;   typed λ calculus, i.e., no ownership)
;;
;; ?> :: stx -> stx
(define-parser ?>
  #:literal-sets [*expr-lits]

  ;; ⊢p P ⇒ ?P : t
  ;;
  ;; `P` elaborates to `?P` with type `t`
  [(*prog ~! CLASS ... E)
   ;; Check no duplicated class names; store them in CS
   #:fail-when (class-twice? #'(CLASS ...)) "Duplicated class name in prog"
   #:and (~do (stx-map CS-set! #'(CLASS ...)))
   #:and (~do (dbg (unbox P)))
   ;; Elaborate
   #:with [?CLASS ...] (stx-map ?> #'(CLASS ...))  ;; P ⊢d CLASS ⇒ ?CLASS
   #:with ?E           (?> #'E)                    ;; P,[] ⊢e E ⇒ ?E : t
   #:with t (dbg (type-prop #'?E))
   ;; ------------------------------------------------------------------
   (type-prop this-syntax #'t)]

  ;; P ⊢d CLASS ⇒ ?CLASS
  ;;
  ;; `CLASS` elaborates to `?CLASS` (under `P`)
  [(*class ~! NAME [CPARAM ...] FIELD/DEF ...)
   #:with [FIELD ...] (filter field? (stx->list #'(FIELD/DEF ...)))
   #:with [DEF ...] (filter def? (stx->list #'(FIELD/DEF ...)))
   ;; Check no duplicated field names; store them in FS
   #:fail-when (field-twice? #'(FIELD ...)) "Duplicated field name in class"
   #:and (~do (stx-map (curry FS-set! #'NAME) #'(FIELD ...)))
   ;; Check no duplicated def names; store them in DS
   #:fail-when (def-twice? #'(DEF ...))     "Duplicated def name in class"
   #:and (~do (stx-map (curry DS-set! #'NAME) #'(DEF ...)))
   ;; Elaboration
   #:with [?FIELD ...] (stx-map ?> #'(FIELD ...))  ;; P ⊢t t
   #:with [?DEF ...] (stx-map ?> #'(DEF ...))
   this-syntax]

  ;; Elaboration on field look a type
  [(*field ~! NAME OWNER TYPE (CPARAM ...))
   this-syntax]

  ;; P,t ⊢p def ⇒ ?def
  ;;
  ;; `def` in `t` elaborates to `?def`
  [(*def ~! (NAME (A-NAME A-OWNER A-TYPE A-CPARAMS) ... (R-OWNER R-TYPE R-CPARAMS)) BODY)
   #:when (?> #'BODY)
   this-syntax]

  ;; P,Γ ⊢e e ⇒ ?e : t
  ;;
  ;; `e` elaborates to `?e` with type `t`
  [(*let ~! (VAR-NAME VAR-OWNER VAR-TYPE VAR-CPARAMS E) BODY)
   #:when (?> #'E)
   #:when (?> #'BODY)
   this-syntax]

  [(*new ~! OWNER TYPE (CPARAM ...))
   this-syntax]

  [(*get-field ~! E FNAME)
   #:with ?E (?> #'E)          ;; Elaborate on E (⊢e)
   #:with ?t (type-prop #'?E)  ;; Get type of ?E
   ;; #:fail-when (∉p #'?t #'FNAME) "Field does not exist in class"
   ;; Tag `this-syntax' with type of FNAME
   ;; #:with [_ *t _] (FS-type #'?t #'FNAME)
   ;; (type-prop this-syntax #'*t)
   this-syntax]

  [(*set-field! ~! E FNAME BODY)
   #:when (?> #'E)
   #:when (?> #'BODY)
   this-syntax]

  [(*send ~! E DNAME PARAM ...)
   #:when (?> #'E)
   #:when (stx-map ?> #'(PARAM ...))
   this-syntax]

  ;; An identifier is either this or a local binding. Class binding
  ;; has been rewritten into `*get-field` during desugaring.
  [*this
   ;; The type of *this is the class type.
   #:with ?c (c-type-prop this-syntax)
   (type-prop this-syntax #'?c)]
  [ID:id #:when (binded? #'ID)
   ;; The type of a local binded is stored in the `binder-prop`.
   ;; #:with [_ ?t _] (dbg (binder-prop #'ID))
   #:with ?t (dbg (ow-type->simple-type (binder-prop #'ID)))
   (type-prop this-syntax #'?t)]
  [ID:id ;; Not locally binded? ⇒ unbound identifier
   (raise-syntax-error #f "unbound identifier" #'ID)])



;; Class expander

;; (define-syntax-parser @%module-begin
;;   [(_ CLASS:expr ... E:expr)
;;    ;; Check `CLASS ...` is a list of class definition
;;    #:when (forall is-class? (syntax->list #'(CLASS ...)))
;;    #'(#%module-begin
;;       (define prog `(prog ,CLASS ... ,E))
;;       (type-check prog)
;;       )])

;; ;; ----- Class
;; ;;  CLASS := (class NAME (C-PARAMS ...) FIELD ... DEF ...)
;; ;;  ~> (class O-SCHEME NAME FIELD ... DEF ...)
;; (define-syntax-parser class
;;   [(_ NAME:id [CPARAM:id ...] BODY:expr ...)
;;    #:when (is-class-body? #'(BODY ...))
;;    #:with O-SCHEME #'(ownership-scheme NAME 'Θ '[CPARAM ...])
;;    #'`(class ,O-SCHEME NAME ,BODY ...)]
;;   [(_ NAME:id BODY:expr ...)
;;    #:when (is-class-body? #'(BODY ...))
;;    #:with O-SCHEME #'(ownership-scheme NAME 'Θ '())
;;    #'`(class ,O-SCHEME NAME ,BODY ...)]
;;   )

;; ;;  FIELD := (field [NAME : TYPE])
;; ;;  ~> (field O-SCHEME NAME)
;; (define-syntax-parser field
;;   [(_ ARG:arg)
;;    #:with O-SCHEME #'(ownership-scheme ARG.TYPE ARG.OWNER ARG.CPARAMS)
;;    #'`(field ,O-SCHEME ,ARG.NAME)])

;; ;;  DEF := (def (NAME [ARG:NAME : ARG:TYPE] ... → RET:TYPE) E)
;; ;;  ~> (def RET:O-SCHEME NAME (ARG:O-SCHEME ...) E)
;; (define-syntax-parser def
;;   #:datum-literals (→)
;;   [(_ (DNAME ARG:arg ... → RET:type)  BODY:expr)
;;    #:with ARG-O-SCHEMEs
;;      #''(([ownership-scheme ARG.TYPE ARG.OWNER ARG.CPARAMS] . ARG.NAME) ...)
;;    #:with RET-O-SCHEME
;;      #'(ownership-scheme RET.TYPE RET.OWNER RET.CPARAMS)
;;    #'`(def ,RET-O-SCHEME DNAME ,ARG-O-SCHEMEs ,BODY)])

;; ;; ---- Expression
;; (define-syntax-parser @let
;;   #:datum-literals (:)
;;   [(_ (ANAME:id : ATYPE:type E:expr) BODY:expr)
;;    #:with O-SCHEME #'(ownership-scheme ATYPE.TYPE ATYPE.OWNER ATYPE.CPARAMS)
;;    #'`(let (,O-SCHEME ANAME ,E) ,BODY)]
;;   [(_ ((ANAME:id : ATYPE:type E:expr) BINDING ...) BODY:expr)
;;    ;; A `@let` with multiple binding into multiple `@let`s.
;;    #:with B1 #'(ANAME : ATYPE E)  ;; The first binding of `@let`
;;    #:with BS #'(BINDING ...)      ;; Remaining bindings of `@let`
;;    (match (syntax-e #'(BINDING ...))
;;      ;; There are no remaining bindings ⇒ reduce it to one last let
;;      [(list)       #'(@let B1 BODY)]
;;      ;; There are remaining bindings ⇒ put a let under with them
;;      [(list x ...) #'(@let B1 (@let BS BODY))])])

;; (define-syntax-parser @%app
;;   #:literal-sets ([keyword-lits])
;;   [(_ new CLASS:type)
;;    #:with O-SCHEME #'(ownership-scheme CLASS.TYPE CLASS.OWNER CLASS.CPARAMS)
;;    #'`(new ,O-SCHEME)]
;;   [(_ get-field CNAME:id FNAME:id)
;;    #'`(get-field CNAME FNAME)]
;;   [(_ set-field! CNAME:id FNAME:id E:expr)
;;    #'`(set-field! CNAME FNAME ,E)]
;;   [(_ send E:expr DNAME:id ARG:expr ...)
;;    #'`(send ,E DNAME ,ARG ...)]
;;   ;; [(_ F:id ARG:expr ...)
;;   ;;  ;; #:fail-unless (is-keyword? #'F)
;;   ;;  ;; ;; (format "unexpected literal, expected one of ~s" keyword-lits)
;;   ;;  ;; "unexpected literal, expected one of ~s"
;;   ;;  #'(#%app F ARG ...)]
;;   )

;; ;; ---- Value
;; ;; (define-syntax-parser @%datum
;; ;;   [(_ . N:nat) #''(Num  N)])


;; (define-syntax-parser @%top
;;   [(_ . ID:id)
;;    #''ID])
;;    ;; #''(Var 'ID)])


;; Utils

;; Returns `#t` if the syntax object is a field.
;; field? : Syntax -> Boolean
(define field?
  (syntax-parser
    #:literal-sets [*expr-lits]
    [(*field _ ...) #t]
    [_ #f]))

;; Returns `#t` if the syntax object is a def.
;; def? : Syntax -> Boolean
(define def?
  (syntax-parser
    #:literal-sets [*expr-lits]
    [(*def _ ...) #t]
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
      [(*class name _ ...) #'name]))

  (let* ([classes     (syntax->list clss-stx)]
         [class-names (map get-class-name classes)])
    (check-duplicate-identifier class-names)))

;; Returns the first duplicate field in the class or #f if there are
;; no duplicate.
(define (field-twice? field-stxs)
  (define get-field-name
    (syntax-parser
      #:literal-sets [*expr-lits]
      [(*field name _ ...) #'name]))

  (define field-names (map get-field-name (stx->list field-stxs)))
  (check-duplicate-identifier field-names))

;; Returns the first duplicate def in the class or #f if there are
;; no duplicate.
(define (def-twice? def-stxs)
  (define get-def-name
    (syntax-parser
      #:literal-sets [*expr-lits]
      [(*def (name _ ...) _) #'name]))

    (define def-names (map get-def-name (stx->list def-stxs)))
    (check-duplicate-identifier def-names))

;; Extracts from an OW-SCHEME a SIMPLE-TYPE
(define ow-type->simple-type (syntax-parser [(_ t _) #'t]))
