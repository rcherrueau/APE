#lang racket/base

(require syntax/parse
         syntax/srcloc
         typed/racket/base
         typed/racket/unsafe)

;; (require/typed racket/list
;;   ;; Coerce `flatten` to only return Listof String (instead of Listof
;;   ;; Any). This helps type-checking the `unlines` function.
;;   [flatten (Any -> (Listof String))])

;; (require/typed syntax/parse
;;   ;; Coerce `flatten` to only return Listof String (instead of Listof
;;   ;; Any). This helps type-checking the `unlines` function.
;;   [syntax-parser (Any -> Any)])

;; (provide (all-defined-out))

;; Provide without the generation of contracts
(unsafe-provide (all-defined-out))

(define-literal-set keyword-lits
  #:for-label
  ;; Note: I have to define new, send, ... as datum otherwise they
  ;; are going to be interpreted as identifier during macro
  ;; expansion and may risk an evaluation with new, send from
  ;; racket/class.
  #:datum-literals (new send get-field set-field! this)
  ;; I have no literals that should be interpreted.
  ())

(define-literal-set expr-lits
  #:for-label
  #:datum-literals (prog class field def let)
  ())

(define-literal-set *expr-lits
  #:for-label
  #:datum-literals (*prog *class *field *def *let *new *send
                    *get-field *set-field! *this)
  ())


(define-syntax-class *ow-scheme
  #:description "Type with ownership and context parameters"
  #:datum-literals [*ow-scheme]
  #:attributes [OWNER CPARAMS TYPE]
  (pattern (*ow-scheme ~! TYPE:id OWNER:id (CPARAM:id ...))
           #:with CPARAMS #'(CPARAM ...)))

(define (make-*ow-scheme stx-src TYPE OWNER CPARAMS)
  (syntax/loc stx-src (*ow-scheme T.TYPE T.OWNER T.CPARAMS)))


(define-type BINDER (Syntaxof Any))
(define-type C-TYPE (Syntaxof Any))
(define-type FIELD (Syntaxof Any))
(define-type DEF (Syntaxof Any))
(define-type TYPE (Syntaxof Any))

(define-type C-type Any)
(define-type F-name Any)
(define-type D-name Any)
(define-type Type Any)

;; Set or get the binder property of a syntax object.
;;
;; The binder property of a syntax object is another syntax object
;; which is its binder.
(: binder-prop
   (All (a) (case->
             [(Syntaxof a) -> BINDER]
             [(Syntaxof a) BINDER -> (Syntaxof a)])))
(define binder-prop
  (case-lambda
    ;; `(Syntaxof a) -> BINDER` requires a `cast` to type check
    ;; because the type-checker doesn't know that 'binder prop of
    ;; `syntax-property` stores BINDER.
    [(stx)        (cast (syntax-property  stx 'binder) BINDER)]
    [(stx BINDER) (syntax-property stx 'binder BINDER)]))

;; Set or get the class type of a syntax object.
(: c-type-prop
   (All (a) (case->
             [(Syntaxof a) -> C-TYPE]
             [(Syntaxof a) C-TYPE -> (Syntaxof a)])))
(define c-type-prop
  (case-lambda
    [(stx)        (cast (syntax-property stx 'c-type) C-TYPE)]
    [(stx C-TYPE) (syntax-property stx 'c-type C-TYPE)]))


;; Set or get the type of a syntax object.
(: type-prop
   (All (a) (case->
             [(Syntaxof a) -> TYPE]
             [(Syntaxof a) TYPE -> (Syntaxof a)])))
(define type-prop
  (case-lambda
    [(stx)      (cast (syntax-property stx 'type) TYPE)]
    [(stx TYPE) (syntax-property stx 'type TYPE)]))


;; List of Types. TODO: Make private
(: CS (Boxof (Listof C-type)))
(define CS (box '()))

(: CS-set! (CLASS -> Void))
(define (CS-set! CLASS)
  (define get-c-type
    (syntax-parser
      #:literal-sets [*expr-lits]
      [(*class ~! NAME [CPARAM ...] FIELD/DEF ...) (syntax->datum #'NAME)]))

  (define C-types (unbox CS))
  (set-box! CS (cons (get-c-type CLASS) C-types)))


;; (*class ~! NAME [CPARAM ...] FIELD/DEF ...)

;; Map of Fields
(: FS (Mutable-HashTable (Pairof C-type F-name) Type))
(define FS (make-hash))

(: FS-set! (C-TYPE FIELD -> Void))
(define (FS-set! C-TYPE FIELD)
  (define get-name/ow-type
    (syntax-parser
      #:literal-sets [*expr-lits]
      [(*field NAME OWNER TYPE (CPARAM ...))
       (values (syntax->datum #'NAME)
               (syntax->datum #'(OWNER TYPE (CPARAM ...))))]))

  (define-values (name ow-type) (get-name/ow-type FIELD))
  (define c-type (syntax->datum C-TYPE))

  (hash-set! FS (cons c-type name) ow-type))

(define (FS-type CLASS F-NAME)
  (define key (cons (syntax->datum CLASS)
                    (syntax->datum F-NAME)))
  (hash-ref FS key))

(define (∉p CLASS F-NAME)
  (define key (cons (syntax->datum CLASS) (syntax->datum F-NAME)))
  (if (hash-has-key? FS key)
      #f CLASS))

;; Map of Defs
(: DS (Mutable-HashTable (Pairof C-type D-name) Type))
(define DS (make-hash))

(: DS-set! (C-TYPE DEF -> Void))
(define (DS-set! C-TYPE DEF)
  (define get-name/ret-ow-type
    (syntax-parser
      #:literal-sets [*expr-lits]
      [(*def ~! (NAME (A-NAME A-OWNER A-TYPE A-CPARAMS) ... (R-OWNER R-TYPE R-CPARAMS)) BODY)
       (values (syntax->datum #'NAME)
               (syntax->datum #'(R-OWNER R-TYPE R-CPARAMS)))]))

  (define-values (name ret-ow-type) (get-name/ret-ow-type DEF))
  (define c-type (syntax->datum C-TYPE))

  (hash-set! DS (cons c-type name) ret-ow-type))
