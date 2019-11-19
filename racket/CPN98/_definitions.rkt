#lang typed/racket/base/no-check

(require racket/dict)

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;;
;; (Untyped) common definitions for parsers of the lang.
;;
;; TODO: move all parts of this file into definitions.rkt

(require syntax/parse
         "utils.rkt")

(provide (all-defined-out))


;; TODO: "scheme" meaning? look at "Hindley-Milner Elaboration in
;; Application" paper, for a definition of scheme.
;;
;; http://gallium.inria.fr/~fpottier/publis/fpottier-elaboration.pdf
(define-syntax-class ow-scheme
  #:description "type with ownership and context parameters"
  #:attributes [OWNER CPARAMS TYPE]
  #:commit
  #:opaque
  (pattern (TYPE:id OWNER:id (CPARAM:id ...))
           #:with CPARAMS #'(CPARAM ...)))

(define (make-ow-scheme TYPE OWNER CPARAMS #:srcloc stx-src)
  (define stx (quasisyntax/loc stx-src (#,TYPE #,OWNER #,CPARAMS)))
  stx)

;; (provide (struct-out immutable-custom-map))
;; (provide (struct-out IdIdMap) make-ididmap ididmap-ref ididmap-set ididmap-has-key?)

;; (define-type IdId (Syntaxof (Pairof Identifier Identifier)))

;; (define-custom-hash-types priv:idid-map
;;   #:key? (λ ([K : Idid])
;;            (if (syntax? K)
;;                (let ([k (syntax-e K)])
;;                  (and (pair? k) (identifier? (car k)) (identifier? (cdr k))))
;;                #f))
;;   (λ ([X : Idid] [Y : Idid])
;;     (let* ([x (syntax-e X)]
;;            [y (syntax-e Y)]
;;            [x1 (car x)] [x2 (cdr x)]
;;            [y1 (car y)] [y2 (cdr y)])
;;       (if (and (eq? (syntax-e x1) (syntax-e y1))
;;                (eq? (syntax-e x2) (syntax-e y2)))
;;           #t #f)))
;;   )

;; (struct IdIdMap ([dict : (Immutable-HashTable IdId Identifier)]))

;; (: make-ididmap
;;    (->* () ((Syntaxof (Listof (Pairof IdId Identifier)))) IdIdMap))
;; (define (make-ididmap [alist '()])
;;   (IdIdMap (make-immutable-priv:idid-map alist)))

;; (: ididmap-ref (IdIdMap IdId -> Identifier))
;; (define (ididmap-ref dict key)
;;   (dict-ref (IdIdMap-dict dict) key))

;; (: ididmap-set
;;    (IdIdMap IdId Identifier -> IdIdMap))
;; (define (ididmap-set dict key val)
;;   (IdIdMap (dict-set (IdIdMap-dict dict) key val)))

;; (: ididmap-has-key?
;;    (IdIdMap IdId -> Boolean))
;; (define (ididmap-has-key? dict key)
;;   (dict-has-key? (IdIdMap-dict dict) key))

;; #:methods gen:dict
;; [(define (dict-ref dict key [default (lambda () (error "key not found" key))])
;;    (dict-ref (IdidMap-dict dict) key default))
;;  (define (dict-set dict key val)
;;    (dict-set (IdidMap-dict dict) key val))
;;  (define (dict-remove dict key)
;;    (dict-remove (IdidMap-dict dict) key))
;;  (define (dict-iterate-first dict)
;;    (dict-iterate-first (IdidMap-dict dict)))
;;  (define (dict-iterate-next dict pos)
;;    (dict-iterate-next (IdidMap-dict dict) pos))
;;  (define (dict-iterate-key dict pos)
;;    (dict-iterate-key (IdidMap-dict dict) pos))
;;  (define (dict-iterate-value dict pos)
;;    (dict-iterate-value (IdidMap-dict dict) pos))
;;  (define (dict-map-key? dict key)
;;    (dict-map-key? (IdidMap-dict dict) key))
;;  (define (dict-count dict)
;;    (dict-count (IdidMap-dict dict)))
;;  ])



;; (define-literal-set keyword-lits
;;   #:for-label
;;   ;; Note: I have to define new, send, ... as datum otherwise they
;;   ;; are going to be interpreted as identifier during macro
;;   ;; expansion and may risk an evaluation with new, send from
;;   ;; racket/class.
;;   #:datum-literals (prog class field def)
;;   ;; I have no literals that should be interpreted.
;;   ())

;; (define-literal-set expr-lits
;;   #:for-label
;;   #:datum-literals (let new send get-field set-field! ???)
;;   ())

;; (define-literal-set *expr-lits
;;   #:for-label
;;   #:datum-literals (prog class field def let new send
;;                          get-field set-field!
;;                          ???)
;;   ())

;; ;; (define *expr-lits? (literal-set->predicate *expr-lits))

;; (define-literal-set *annotation-lits
;;   #:for-label
;;   #:datum-literals (ow-scheme)
;;   ())

;; 
;; (define-syntax-class ow-scheme
;;   #:description "type with ownership and context parameters"
;;   #:literal-sets [*annotation-lits]
;;   #:attributes [OWNER CPARAMS TYPE]
;;   (pattern (ow-scheme ~! TYPE:id OWNER:id (CPARAM:id ...))
;;            #:with CPARAMS #'(CPARAM ...))
;;   )



;; (define-type BINDER (Syntaxof Any))
;; (define-type C-TYPE (Syntaxof Any))
;; (define-type FIELD (Syntaxof Any))
;; (define-type DEF (Syntaxof Any))
;; (define-type TYPE (Syntaxof Any))

;; (define-type C-type Any)
;; (define-type F-name Any)
;; (define-type D-name Any)
;; (define-type Type Any)

;; Set or get the binder property of a syntax object.
;;
;; The binder property of a syntax object is another syntax object
;; which is its binder.
;; (: binder-prop
;;    (All (a) (case->
;;              [(Syntaxof a) -> BINDER]
;;              [(Syntaxof a) BINDER -> (Syntaxof a)])))
;; (define binder-prop
;;   (case-lambda
;;     ;; `(Syntaxof a) -> BINDER` requires a `cast` to type check
;;     ;; because the type-checker doesn't know that 'binder prop of
;;     ;; `syntax-property` stores BINDER.
;;     [(stx)        (cast (syntax-property  stx 'binder) BINDER)]
;;     [(stx BINDER) (syntax-property stx 'binder BINDER)]))

;; ;; Set or get the class type of a syntax object.
;; (: c-type-prop
;;    (All (a) (case->
;;              [(Syntaxof a) -> C-TYPE]
;;              [(Syntaxof a) C-TYPE -> (Syntaxof a)])))
;; (define c-type-prop
;;   (case-lambda
;;     [(stx)        (cast (syntax-property stx 'c-type) C-TYPE)]
;;     [(stx C-TYPE) (syntax-property stx 'c-type C-TYPE)]))


;; ;; Set or get the type of a syntax object.
;; (: type-prop
;;    (All (a) (case->
;;              [(Syntaxof a) -> TYPE]
;;              [(Syntaxof a) TYPE -> (Syntaxof a)])))
;; (define type-prop
;;   (case-lambda
;;     [(stx)      (cast (syntax-property stx 'type) TYPE)]
;;     [(stx TYPE) (syntax-property stx 'type TYPE)]))


;; Environments

;;~~~~~~~~~~~~~~~~~~
;; FS: Map of Fields

;; ;; Map of Fields
;; (: FS (Mutable-HashTable (Pairof C-type F-name) Type))
;; (define FS (make-hash))

;; ;; Removes all mapping of Fields
;; (: FS-clear! (-> Void))
;; (define (FS-clear!) (hash-clear! FS))

;; (: FS-set! (C-TYPE FIELD -> Void))
;; (define (FS-set! C-TYPE FIELD)
;;   (define get-name/ow-type
;;     (syntax-parser
;;       #:literal-sets [*expr-lits]
;;       [(field NAME OWS:ow-scheme)
;;        (values (syntax->datum #'NAME) #'OWS)]))

;;   (define c-type (syntax->datum C-TYPE))
;;   (define-values (name ow-type) (get-name/ow-type FIELD))

;;   (hash-set! FS (cons c-type name) ow-type))

;; (define (FS-type E F-NAME #:context? [CONTEXT? #f])
;;   (define CLASS (type-prop E))
;;   (define class (syntax->datum CLASS))
;;   (define field (syntax->datum F-NAME))
;;   (define error-msg "type ~s doesn't have field ~s")

;;   (hash-ref FS (cons class field)
;;             (λ () (raise-syntax-error
;;                    #f
;;                    (format error-msg class field)
;;                    CONTEXT?
;;                    E
;;                    ))))

;; (define (∉p CLASS F-NAME)
;;   (define key (cons (syntax->datum CLASS) (syntax->datum F-NAME)))
;;   (if (hash-has-key? FS key)
;;       #f CLASS))

;; ;; Map of Defs
;; (: DS (Mutable-HashTable (Pairof C-type D-name) Type))
;; (define DS (make-hash))

;; ;; Removes all mapping of Defs
;; (: DS-clear! (-> Void))
;; (define (DS-clear!) (hash-clear! DS))

;; (: DS-set! (C-TYPE DEF -> Void))
;; (define (DS-set! C-TYPE DEF)
;;   (define get-name/ret-ow-type
;;     (syntax-parser
;;       #:literal-sets [*expr-lits]
;;       ;; [(def ~! (NAME (A-NAME A-OWS:ow-scheme) ... R-OWS:*ow-scheme) BODY)
;;       [(def ~! (NAME _ ... R-OWS:ow-scheme) BODY)
;;        (values (syntax->datum #'NAME) #'R-OWS)]))

;;   (define c-type (syntax->datum C-TYPE))
;;   (define-values (name ret-ow-type) (get-name/ret-ow-type DEF))

;;   (hash-set! DS (cons c-type name) ret-ow-type))

;; (define (DS-type CLASS DEF-NAME)
;;   (define key (cons (syntax->datum CLASS)
;;                     (syntax->datum DEF-NAME)))
;;   (hash-ref DS key))
