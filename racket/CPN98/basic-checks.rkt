#lang racket/base

(require racket/match
         racket/function
         racket/list
         racket/pretty
         racket/syntax
         "utils.rkt"
         "definitions.rkt"
         syntax/parse
         syntax/parse/define
         syntax/module-reader
         syntax/stx
         (for-syntax syntax/parse
                     syntax/parse/define
                     racket/base
                     "definitions.rkt"
                     )
         )


;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects


;; Checking transformation (?>)
;;
;; - Checks no duplicate class name, field, def
;; - Type checks the program (for vanilla type -- no ownership)
(provide ?>)

;; ?> :: stx -> stx
(define ?> (syntax-parser
  #:literal-sets [*keyword-lits *expr-lits]

  [(*prog ~! CLASS ... E)
   #:fail-when (check-class #'(CLASS ...)) "Duplicated class name"
   #:when (stx-map ?> #'(CLASS ...))
   #:when (?> #'E)
   this-syntax]

  [(*class ~! NAME [CPARAM ...] FIELD/DEF ...)
   #:fail-when (field-twice? #'(FIELD/DEF ...)) "Duplicated field"
   #:fail-when (def-twice? #'(FIELD/DEF ...))   "Duplicated def"
   #:when (stx-map ?> #'(FIELD/DEF ...))
   this-syntax]

  [(*field ~! NAME OWNER TYPE (CPARAM ...))
   this-syntax]

  [(*def ~! (NAME (A-NAME A-OWNER A-TYPE A-CPARAMS) ... (R-OWNER R-TYPE R-CPARAMS)) BODY)
   #:when (?> #'BODY)
   this-syntax]

  [(*let ~! (VAR-NAME VAR-OWNER VAR-TYPE VAR-CPARAMS E) BODY)
   #:when (?> #'E)
   #:when (?> #'BODY)
   this-syntax]

  [(*new ~! OWNER TYPE (CPARAM ...))
   this-syntax]

  [(*get-field ~! E FNAME)
   #:when (?> #'E)
   this-syntax]

  [(*set-field! ~! E FNAME BODY)
   #:when (?> #'E)
   #:when (?> #'BODY)
   this-syntax]

  [(*send ~! E DNAME PARAM ...)
   #:when (?> #'E)
   #:when (stx-map ?> #'(PARAM ...))
   this-syntax]

  ;; An identifier is either this or a local binding, class-binding
  ;; has been rewrite into `*get-field` during Desugaring.
  [*this
   this-syntax]
  [ID:id #:when (binded? #'ID)
   this-syntax]
  [ID:id ;; Not locally binded? ⇒ unbound identifier
   (raise-syntax-error #f "unbound identifier" #'ID)]))



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
