#lang racket/base

(require racket/function
         syntax/parse
         syntax/srcloc
         ;; dbg macro.
         ;; TODO: remove this
         (for-syntax racket/base
                     racket/pretty
                     racket/port)
         syntax/parse/define
         ;; END TODO
         typed/racket/base
         typed/racket/unsafe
         )

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


;; (dbg (+ 1 2))
(define-syntax-parser dbgg
  [(_ E:expr)
   #`(let ([src    #,(syntax-source #'E)]
           [line   #,(syntax-line #'E)]
           [col    #,(syntax-column #'E)]
           [datum  #,(call-with-output-string
                      (λ (out-str) (pretty-print (syntax->datum #'E) out-str #:newline? #f)))]
           [res    E])
       (define-values (base file _) (split-path src))
       (printf "; [dbg] ~a:~s:~s: ~a = ~s~n" file line col datum res)
       res)])

(define-literal-set keyword-lits
  #:for-label
  ;; Note: I have to define new, send, ... as datum otherwise they
  ;; are going to be interpreted as identifier during macro
  ;; expansion and may risk an evaluation with new, send from
  ;; racket/class.
  #:datum-literals (prog class field def)
  ;; I have no literals that should be interpreted.
  ())

(define-literal-set expr-lits
  #:for-label
  #:datum-literals (let new send get-field set-field! ???)
  ())

(define-literal-set *expr-lits
  #:for-label
  #:datum-literals (prog class field def let new send
                         get-field set-field!
                          this ???)
  ())

(define *expr-lits? (literal-set->predicate *expr-lits))

(define-literal-set *annotation-lits
  #:for-label
  #:datum-literals (ow-scheme)
  ())

(define-syntax-class ow-scheme
  #:description "type with ownership and context parameters"
  #:literal-sets [*annotation-lits]
  #:attributes [OWNER CPARAMS TYPE]
  (pattern (ow-scheme ~! TYPE:id OWNER:id (CPARAM:id ...))
           #:with CPARAMS #'(CPARAM ...))
  )

(define (make-ow-scheme TYPE OWNER CPARAMS #:stx-src [stx-src #f])
  (define ow-scheme-stx #`(ow-scheme #,TYPE #,OWNER #,CPARAMS))

  (if stx-src
    (quasisyntax/loc stx-src #,ow-scheme-stx)
    ow-scheme-stx))


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
      [(class ~! NAME [CPARAM ...] FIELD/DEF ...) (syntax->datum #'NAME)]))

  (define C-types (unbox CS))
  (set-box! CS (cons (get-c-type CLASS) C-types)))

(: CS-member (Class -> Void))
(define (CS-member CLASS)
  (define class (syntax->datum CLASS))
  (member class (unbox CS)))

;; (*class ~! NAME [CPARAM ...] FIELD/DEF ...)

;; Map of Fields
(: FS (Mutable-HashTable (Pairof C-type F-name) Type))
(define FS (make-hash))

(: FS-set! (C-TYPE FIELD -> Void))
(define (FS-set! C-TYPE FIELD)
  (define get-name/ow-type
    (syntax-parser
      #:literal-sets [*expr-lits]
      [(field NAME OWS:ow-scheme)
       (values (syntax->datum #'NAME) #'OWS)]))

  (define c-type (syntax->datum C-TYPE))
  (define-values (name ow-type) (get-name/ow-type FIELD))

  (hash-set! FS (cons c-type name) ow-type))

(define (FS-type E F-NAME #:context? [CONTEXT? #f])
  (define CLASS (type-prop E))
  (define class (syntax->datum CLASS))
  (define field (syntax->datum F-NAME))
  (define error-msg "type ~s doesn't have field ~s")

  (hash-ref FS (cons class field)
            (λ () (raise-syntax-error
                   #f
                   (format error-msg class field)
                   CONTEXT?
                   E
                   ))))

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
      ;; [(def ~! (NAME (A-NAME A-OWS:ow-scheme) ... R-OWS:*ow-scheme) BODY)
      [(def ~! (NAME _ ... R-OWS:ow-scheme) BODY)
       (values (syntax->datum #'NAME) #'R-OWS)]))

  (define c-type (syntax->datum C-TYPE))
  (define-values (name ret-ow-type) (get-name/ret-ow-type DEF))

  (hash-set! DS (cons c-type name) ret-ow-type))

(define (DS-type CLASS DEF-NAME)
  (define key (cons (syntax->datum CLASS)
                    (syntax->datum DEF-NAME)))
  (hash-ref DS key))
