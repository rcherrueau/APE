#lang racket/base

;; @InProceedings{CPN98,
;;   author    = {David G. Clarke and
;;                John Potter and
;;                James Noble},
;;   title     = {Ownership Types for Flexible Alias Protection},
;;   booktitle = {Proceedings of the 1998 {ACM} {SIGPLAN} Conference
;;                on Object-Oriented Programming Systems, Languages
;;                {\&} Applications {(OOPSLA} '98),
;;                Vancouver, British Columbia, Canada, October 18-22, 1998.},
;;   pages     = {48--64},
;;   year      = {1998},
;;   url       = {https://doi.org/10.1145/286936.286947},
;;   doi       = {10.1145/286936.286947}
;; }

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/match
                     "type-checker.rkt"
                     )
         syntax/parse
         syntax/parse/define
         racket/match
         ;;
         "type-checker.rkt"
         )

(provide (rename-out
          [surface-read read]
          [surface-read-syntax read-syntax]

          ;; [@%module-begin #%module-begin]
          ;;            [@%top #%top] ;; wrapped unbound variables
          ;;            [@let let]
          ;;            [@%app #%app]
          ;;            [@class class]
          ;;            ;; [@field field] [@def def]
          ;;            ;; [@%datum #%datum]
                     )
         ;; #%app #%top
         ownership-scheme
         #%top-interaction)

;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects



(define (surface-read in)
  (syntax->datum (surface-read-syntax #f in)))

;; (define-syntax-parser @class
;;   [(_ NAME:id [CPARAM:id ...] BODY:expr ...)
;;    #:when (is-class-body? #'(BODY ...))
;;    #'`(class NAME [CPARAM ...] ,BODY ...)]
;;   [(_ NAME:id BODY:expr ...)
;;    #:when (is-class-body? #'(BODY ...))
;;    #'(@class NAME [] BODY ...)]
;;   )
;; (define-syntax-parser @let
;;   #:datum-literals (:)
;;   [(_ (ANAME:id : ATYPE:type E:expr) BODY:expr)
;;    #'`(let (ANAME : ATYPE ,E) ,BODY)]
;;   [(_ ((ANAME:id : ATYPE:type E:expr) BINDING ...) BODY:expr)
;;    ;; A `@let` with multiple binding into multiple `@let`s.
;;    ;; #:with B1 #'(ANAME : ATYPE E)  ;; The first binding of `@let`
;;    ;; #:with BS #'(BINDING ...)      ;; Remaining bindings of `@let`
;;    (syntax-case #'(BINDING ...) ()
;;      [()      #'(@let (ANAME : ATYPE E) BODY)]
;;      [(x ...) #'(@let (ANAME : ATYPE E) (@let (x ...) BODY))]
;;                  )])
;;    ;; (match (syntax-e #'(BINDING ...))
;;    ;;   ;; There are no remaining bindings ⇒ reduce it to one last let
;;    ;;   [(list)       #'`(let ,B1 ,BODY)]
;;    ;;   ;; There are remaining bindings ⇒ put a let under with them
;;    ;;   [(list x ...) #'(@let B1 (@let BS BODY))])])

(define (surface-read-syntax src in)
  (define (desugar-stx stx)
    (syntax-case stx (class let :)
      [(class NAME BODY ...)
       #`(class NAME [] #,(map desugar-stx (syntax->list #'(BODY ...))))]
      [(let ((ANAME : ATYPE E) BINDING ...) BODY)
       ;; A `let` with multiple binding into multiple `let`s.
       (syntax-case #'(BINDING ...) ()
         [()      #'(let (ANAME : ATYPE E) BODY)]
         [(x ...) #'(let (ANAME : ATYPE E) (let (x ...) BODY))])]
      [_ stx]))

  (define (desugar)
    (let ([s-exp (read-syntax src in)])
      (cond
        [(eof-object? s-exp) '()]
        [else (cons (desugar-stx s-exp) (desugar))])
      ))
  ;;   (match (read-syntax src in)
  ;;     [eof '()]
  ;;     [_ (cons sexp (desugar))]
  ;;   ))
  ;; (letrec ([desugar (λ () (read-syntax src in))])
  ;;   ()
  ;;   )

  ;; TODO: use `read-syntax' of s-exp then parten match on it
  (define prog (desugar))
  (displayln prog)

  #`(module m "surface-desugar-lang.rkt" #,@prog)
  ;; (error "lala")

  )



;; Utils

(begin-for-syntax

  ;; -- Syntax checker
  (define (forall p l)
    (for/and ([i l]) (p i)))

  (define (is-syntax-value? value stx)
    (define expr (syntax-e (car (syntax->list stx))))
    (equal? expr value))

  (define (is-class? stx)
    (is-syntax-value? 'class stx))

  (define (is-field? stx)
    (is-syntax-value? 'field stx))

  (define (is-def? stx)
    (is-syntax-value? 'def stx))

  (define (is-field/def? stx)
    (or (is-field? stx) (is-def? stx)))

  (define (is-class-body? BODY)
    ;; Check `BODY ...` is made of fields and defs
    (forall is-field/def? (syntax->list BODY)))

  (define-literal-set keyword-lits
    ;; Note: I have to define new, send, ... as datum otherwise they
    ;; are going to be interpreted as identifier during macro
    ;; expansion and may risk an evaluation with new, send from
    ;; racket/class.
    #:datum-literals (new send get-field set-field!)
    ;; I have no literals that should be interpreted.
    ())

  ;; Check `#%app F ARG ...` is one of defined keywords
  (define is-keyword? (literal-set->predicate keyword-lits))

  ;; (define (is-expr? E)
  ;;   ;; Check `E` is an expression
  ;;   (define expr (syntax-e E))
  ;;   (cond
  ;;     [(symbol? expr) #t]  ;; variable
  ;;     [(number? expr) #t]
  ;;     [else
  ;;      (let ([expr (syntax-e (car expr))])
  ;;        (member expr '(new let send get-field set-field!)))]))

  ;; -- Syntax class for type and arg
  (define-syntax-class type
    #:description "class' type with ownership and context parameters"
    #:attributes [TYPE OWNER CPARAMS]
    #:datum-literals (/)  ;; Don't consider '/' as a pattern
    (pattern (O:id / T:id)
             #:with OWNER #'O
             #:with TYPE #'T
             #:with CPARAMS #''())
    (pattern (O:id / (T:id PARAMS:id ...+))
             #:with OWNER #'O
             #:with TYPE #'T
             #:with CPARAMS #''(PARAMS ...))
    (pattern T:id
             #:with OWNER #''Θ
             #:with TYPE #'T
             #:with CPARAMS #''())
    (pattern (T:id PARAMS:id ...+)
             #:with OWNER #''Θ
             #:with TYPE #'T
             #:with CPARAMS #''(PARAMS ...))
    )

  (define-syntax-class arg
    #:description "argument with its type"
    #:datum-literals (:)
    (pattern (NAME:id : T:type)
             #:attr OWNER #'T.OWNER
             #:attr TYPE  #'T.TYPE
             #:attr CPARAMS #'T.CPARAMS))

  ;; Format TYPE
  ;; (define (fTYPE TYPE)
  ;;   (syntax-parse TYPE
  ;;     [(t:type) #'(t.TYPE t.OWNER t.CPARAMS)]
  ;;     [(a:arg) #'(a.TYPE a.OWNER a.CPARAMS)]))

  )


;; Class expander

;; ----- Class
;;  CLASS := (class NAME (C-PARAMS ...) FIELD ... DEF ...)
;;  ~> (class O-SCHEME NAME FIELD ... DEF ...)
;; (define-syntax-parser @class
;;   [(_ NAME:id [CPARAM:id ...] BODY:expr ...)
;;    #:when (is-class-body? #'(BODY ...))
;;    #'`(class NAME [CPARAM ...] ,BODY ...)]
;;   [(_ NAME:id BODY:expr ...)
;;    #:when (is-class-body? #'(BODY ...))
;;    #'(@class NAME [] BODY ...)]
;;   )

;; ;;  FIELD := (field [NAME : TYPE])
;; ;;  ~> (field O-SCHEME NAME)
;; ;; (define-syntax-parser @field
;; ;;   [(_ ARG:arg)
;; ;;    #'(field ARG:arg)])

;; ;;  DEF := (def (NAME [ARG:NAME : ARG:TYPE] ... → RET:TYPE) E)
;; ;;  ~> (def RET:O-SCHEME NAME (ARG:O-SCHEME ...) E)
;; ;; (define-syntax-parser @def
;; ;;   #:datum-literals (→)
;; ;;   [(_ (DNAME ARG:arg ... → RET:type)  BODY:expr)
;; ;;    #:with ARG-O-SCHEMEs
;; ;;      #''(([ownership-scheme ARG.TYPE ARG.OWNER ARG.CPARAMS] . ARG.NAME) ...)
;; ;;    #:with RET-O-SCHEME
;; ;;      #'(ownership-scheme RET.TYPE RET.OWNER RET.CPARAMS)
;; ;;    #'`(def ,RET-O-SCHEME DNAME ,ARG-O-SCHEMEs ,BODY)])

;; ;; ---- Expression
;; (define-syntax-parser @let
;;   #:datum-literals (:)
;;   [(_ (ANAME:id : ATYPE:type E:expr) BODY:expr)
;;    #'`(let (ANAME : ATYPE ,E) ,BODY)]
;;   [(_ ((ANAME:id : ATYPE:type E:expr) BINDING ...) BODY:expr)
;;    ;; A `@let` with multiple binding into multiple `@let`s.
;;    ;; #:with B1 #'(ANAME : ATYPE E)  ;; The first binding of `@let`
;;    ;; #:with BS #'(BINDING ...)      ;; Remaining bindings of `@let`
;;    (syntax-case #'(BINDING ...) ()
;;      [()      #'(@let (ANAME : ATYPE E) BODY)]
;;      [(x ...) #'(@let (ANAME : ATYPE E) (@let (x ...) BODY))]
;;                  )])
;;    ;; (match (syntax-e #'(BINDING ...))
;;    ;;   ;; There are no remaining bindings ⇒ reduce it to one last let
;;    ;;   [(list)       #'`(let ,B1 ,BODY)]
;;    ;;   ;; There are remaining bindings ⇒ put a let under with them
;;    ;;   [(list x ...) #'(@let B1 (@let BS BODY))])])

;; (define-syntax-parser @%app
;;   [(_ F:id ARG:expr ...)
;;    ;; #:fail-unless (is-keyword? #'F)
;;    ;; ;; (format "unexpected literal, expected one of ~s" keyword-lits)
;;    ;; "unexpected literal, expected one of ~s"
;;    #'`(F ,ARG ...)]
;;   )

;; ;; ---- Value
;; ;; (define-syntax-parser @%datum
;; ;;   [(_ . N:nat) #''(Num  N)])


;; (define-syntax-parser @%top
;;   [(_ . ID:id)
;;    #''ID])
;;    ;; #''(Var 'ID)])

;; (define-syntax-parser @%module-begin
;;   [(_ CLASS:expr ... E:expr)
;;    ;; Check `CLASS ...` is a list of class definition
;;    #:when (forall is-class? (syntax->list #'(CLASS ...)))
;;    #:with PROG (local-expand #'(CLASS ... E)
;;                              'expression
;;                              ;; #f
;;                              (list)
;;                              ;; (list #'class)
;;                              )

;;    #`(#%plain-module-begin
;;       (#%require "")
;;       (datum->syntax #f #'PROG))

;;    ;; (syntax-case #'PROG ()
;;    ;;   [(module nm lng (#%plain-module-begin . body))
;;    ;;    #`(#%plain-module-begin
;;    ;;       (#%require lng)
;;    ;;       . #,((make-syntax-introducer) #'body))
;;    ;;    ])
;;    ;; #'(#%module-begin
;;    ;;    (module surface-module "surface-desugar-lang.rkt"
;;    ;;      prog))
;;    ])
