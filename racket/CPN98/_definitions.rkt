#lang typed/racket/base/no-check

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;;
;; (Untyped) common definitions for parsers of the lang.
;;
;; TODO: move all parts of this file into definitions.rkt

(require (for-syntax racket/base)
         syntax/parse
         syntax/parse/define
         "utils.rkt")

(provide (all-defined-out))


(define-literal-set keyword-lits
  #:for-label
  ;; Note: The syntax/parse package require all literals to have a
  ;; binding. To match identifier by their symbolic names, I have to
  ;; use `#:datum-literals` instead.
  #:datum-literals (prog class field def)
  ;; I have no literals that should be interpreted.
  ())

(define-literal-set expr-lits
  #:for-label
  #:datum-literals (let new send get-field set-field! this ???)
  ())


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


;; Macro for a new transformation
;;
;; > (define-parser ?>
;; >   #:literal-sets [keyword-lits expr-lits]
;; >
;; >   ;; Environment variables
;; >   #:CS '(...)
;; >   #:FS '(...)
;; >   #:DS '(...)
;; >
;; >   ;; Clauses ...
;; >   [(class NAME FIELD/DEF ...) #'(...)] ...)
;; (define-syntax (define-parser parser-stx)
;;   ;; Strip the `#:` from a (Syntaxof Keyword)
;;   (define (strip#: KEYWORD)
;;     (define keyword (keyword->string (syntax-e KEYWORD)))
;;     (datum->syntax KEYWORD (string->symbol keyword)))

;;   ;; Make the parser
;;   (syntax-parse parser-stx
;;     [(_ ID:id                          ;; Parser name (e.g., `?>`)
;;         #:literal-sets ls              ;; Literals
;;         (~seq K:keyword Env:expr) ...  ;; Environment variables
;;         CLAUSE ...+)                   ;; Clauses of the transfo
;;      #:with [def-K ...] (map strip#: (syntax->list #'(K ...)))
;;      #:with PARSER-ID         (generate-temporary)
;;      #'(begin
;;          ;; Define environment variables as global parameter
;;          (define def-K (make-parameter #f)) ...
;;          (define topCall (make-parameter #t))

;;          ;; Define parser as an internal def
;;          (define PARSER-ID
;;            (syntax-parser
;;              #:literal-sets ls
;;              CLAUSE ...))

;;          ;; Define the parser as a global definition
;;          (define (ID stx)

;;            ;; Parameterize the parser call with `Env` values at the
;;            ;; top Call (not recursive ones)
;;            (if (topCall)
;;                (parameterize ([def-K Env] ... [topCall #f]) (PARSER-ID stx))
;;                (PARSER-ID stx))))]))

(define-syntax (define-parser parser-stx)
  (syntax-parse parser-stx
    [(_ ID:id                         ;; Parser name (e.g., `?>`)
        #:Env ([NAME:id E:expr] ...)  ;; Environment variables
        #:Rules (RHS ...+)            ;; Rules of the transformation
        DEF ...)                      ;; Extra rules as `define`
     #'(begin
         ;; Define environment variables as global parameter
         (define NAME (make-parameter #f)) ...

         ;; Define the parser as a global definition. We parameter the
         ;; first call of rules  with `Env` values
         (define (ID stx)

           ;; Define rules as a local def (overriding name `ID`)
           (define ID (syntax-parser RHS ...))

           ;; Extra rules
           DEF ...

           ;; Parameter the first call of rules with `Env` values
           (parameterize ([NAME E] ...) (ID stx))))]
    [(_ (ID:id stx:expr)              ;; Parser name (e.g., `?>`) and its syntax object/id
        #:Env ([NAME:id E:expr] ...)  ;; Environment variables ...
        DEF)                          ;; parser definition
     #'(begin
         ;; Define environment variables as parameters
         (define NAME (make-parameter #f)) ...

         ;; Define the parser as a global definition. We parameterize
         ;; the first call of `DEF` with `Env` values
         (define (ID stx)
           (parameterize ([NAME E] ...) DEF)))]
    ))

(define-syntax define-rules
  (syntax-parser
    [(_ ID:id RULE:expr ...)
     #'(define (ID stx)
         ;; (dbg stx #:ctx ID)
         (syntax-parse stx
           #:literal-sets [(keyword-lits #:at ID)
                           (expr-lits #:at ID)]
           RULE ...
           ))]))


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
