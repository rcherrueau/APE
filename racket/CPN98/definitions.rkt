#lang typed/racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;;
;; Common definitions for parsers of the lang.

(require (for-syntax racket/base)
         ;; racket/dict
         racket/function
         syntax/parse
         ;; syntax/srcloc
         ;; syntax/parse/define
         ;; dbg macro.
         ;; TODO: remove this
         ;; (for-syntax racket/base
         ;;             racket/pretty
         ;;             racket/port)
         ;; END TODO
         typed/racket/unsafe
         "_definitions.rkt"
         )


;; (define-type PairofId (Pairof Identifier Identifier))
(define-type IdId        (Syntaxof (Pairof Identifier Identifier)))
(define-type (IdIdMap a) (Listof (Pairof IdId a)))
(define-type OW-SCHEME   (Syntaxof
                          (List Identifier                         ;; Basic Type
                                Identifier                         ;; Owner
                                (Syntaxof (Listof Identifier)))))  ;; C-Params


;; (require/typed syntax/parse
;;   ;; Coerce `flatten` to only return Listof String (instead of Listof
;;   ;; Any). This helps type-checking the `unlines` function.
;;   [syntax-parser (Any -> Any)])
(require/typed "utils.rkt"
  [ididmap-ref (All (a) ((IdIdMap a) IdId -> a))]
  [ididmap-set (All (a) ((IdIdMap a) IdId a -> (IdIdMap a)))]
  [ididmap-has-key? (All (a) ((IdIdMap a) IdId -> Boolean))]
  [ididmap-eq? (All (a) (->* ((IdIdMap a) (IdIdMap a)) ((a a -> Boolean)) Boolean))]
  [check-stx=? (->* (Syntax Syntax) (String) Any)])

(provide (except-out (all-defined-out)
                     keyword-lits expr-lits
                     private:with-CS with-CS
                     ;; FIXME: make them private and define a proper
                     ;; interface to define them in order to ensure
                     ;; type safety with contracts.
                     private:CS private:FS
                     )
         (all-from-out "_definitions.rkt"))

;; Provide without the generation of contracts
;;
;; Macros defined in typed modules may not be used in untyped modules.
;; A workaround for such macros is provided them with `unsafe-provide`
;; which exports the macro without any contracts generated. See,
;; https://docs.racket-lang.org/ts-guide/typed-untyped-interaction.html#%28part._.Using_.Typed_.Code_in_.Untyped_.Code%29
;; https://groups.google.com/d/msg/racket-users/eowl6RpdDwY/1wrCluDcAwAJ
(unsafe-provide keyword-lits expr-lits
                private:CS private:FS
                with-CS)


;; Language definitions

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
  #:datum-literals (let new send get-field set-field! ???)
  ())

;; > (ow-scheme? #'(a b ()))       ; #t
;; > (ow-scheme? #'(a b [c d e]))  ; #t
;; > (ow-scheme? #'(a 1 []))       ; #f
(define-predicate ow-scheme? OW-SCHEME)


;; (define (make-ow-scheme OW-SCHEME OWNER CPARAMS #:stx-src [stx-src #f])
;;   (define ow-scheme-stx #`(ow-scheme #,OW-SCHEME #,OWNER #,CPARAMS))

;;   (if stx-src
;;     (quasisyntax/loc stx-src #,ow-scheme-stx)
;;     ow-scheme-stx))

;; ;; Returns `#t` if the syntax object is a def.
;; ;; def? : Syntax -> Boolean
;; (define def?
;;   (syntax-parser
;;     ;; #:literal-sets [expr-lits]
;;     [(def _ ...) #t]
;;     [_ #f]))


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


;; Get or set the type of a syntax object.
(: type-prop
   (All (a) (case->
             [(Syntaxof a) -> (U OW-SCHEME #f)]
             ;; FIXME: remove `U ... (Syntaxof #f)` at the end of
             ;; prototyping. Right now, this enables the propagation
             ;; of not typed term. But it should disappear at the end
             ;; of the prototyping.
             ;;
             ;; [(Syntaxof a) OW-SCHEME -> (Syntaxof a)])))
             [(Syntaxof a) (U OW-SCHEME (Syntaxof #f)) -> (Syntaxof a)])))
(define type-prop
  (case-lambda
    ;; Get the type of a syntax object
    [(stx) (let ([τ (syntax-property stx 'type)])
             (if (ow-scheme? τ) τ #f))]
    ;; Set the type of a syntax object
    [(stx OW-SCHEME) (syntax-property stx 'type OW-SCHEME)]))


;; Environments

(: bound-id=? (Identifier Identifier -> Boolean))
(define (bound-id=? id1 id2)
  (eq? (syntax-e id1) (syntax-e id2)))

;;~~~~~~~~~~~~~~~~~
;; CS: Set of Types

(: private:CS (Parameterof (Listof Identifier)))
(define private:CS (make-parameter '()))

;; Is Identifier exists in CS?
(: CS-member? (Identifier ->  Boolean))
(define (CS-member? id-stx)
  (if (findf (curry bound-id=? id-stx) (private:CS)) #t #f))

;; Make `the-CS` the value of `CS` in `A`.
(: private:with-CS
   (All (A) (U (Syntaxof (Listof Identifier)) (Listof Identifier)) (-> A) -> A))
(define (private:with-CS the-CS thunk-E)
  (parameterize
      ([private:CS
        (cond
          [(and (syntax? the-CS) (syntax->list the-CS)) => identity]
          [else the-CS])])
    ;; (dbg (CS))
    (thunk-E)))

;; Automatically create the `thunk` around E expressions
(define-syntax (with-CS stx)
  (syntax-case stx ()
    [(_ THE-CS E ...) #'(private:with-CS THE-CS (thunk E ...))]))

(module+ test/CS
  (require typed/rackunit)

  (with-CS #'(foo bar)
    (check-true  (CS-member? #'foo))
    (check-false (CS-member? #'baz)))

  (with-CS (list #'foo #'bar)
    (check-true  (CS-member? #'foo))
    (check-false (CS-member? #'baz))))

;;~~~~~~~~~~~~~~~~~~~
;; FS: Map of Fields
;;
;; The syntax pair #'(Class-Type . Field-name) indexes the Map of
;; Fields, and contains the Field type as value.
(define-type FS (IdIdMap OW-SCHEME))

(: private:FS (Parameterof FS))
(define private:FS (make-parameter '()))

(: FS-member? (IdId -> Boolean))
(define (FS-member? C-TYPE.FIELD)
  (ididmap-has-key? (private:FS) C-TYPE.FIELD))

(: FS-set (IdId OW-SCHEME -> FS))
(define (FS-set C-TYPE.FIELD OW-SCHEME)
  (ididmap-set (private:FS) C-TYPE.FIELD OW-SCHEME))

(: FS-ref (IdId -> OW-SCHEME))
(define (FS-ref C-TYPE.FIELD)
  (ididmap-ref  (private:FS) C-TYPE.FIELD))

(module+ test/FS
  (require typed/rackunit)

  (define test:τFoo #'(foo-type rep ()))
  (define test:τBar #'(bar-type rep ()))

  (: test:FS FS)
  (define test:FS
    `((,#'(c . foo) . ,test:τFoo)
      (,#'(c . bar) . ,test:τBar)))

  (: stx-eq? (OW-SCHEME OW-SCHEME -> Boolean))
  (define (stx-eq? a b)
    (equal? (syntax->datum a) (syntax->datum b)))

  (parameterize ([private:FS test:FS])
    (check-true  (FS-member? #'(c . foo)))
    (check-false (FS-member? #'(C . foo)))
    (check-false (FS-member? #'(c . Foo)))
    (check-false (FS-member? #'(c . baz)))

    (check-stx=? (FS-ref #'(c . foo)) test:τFoo)
    (check-stx=? (FS-ref #'(c . bar)) test:τBar)

    (check-true  (ididmap-eq? test:FS (FS-set #'(c . foo) test:τFoo) stx-eq?))
    (check-false (ididmap-eq? test:FS (FS-set #'(C . foo) test:τFoo) stx-eq?))
    (check-false (ididmap-eq? test:FS (FS-set #'(c . Foo) test:τFoo) stx-eq?))
    (check-false (ididmap-eq? test:FS (FS-set #'(c . foo) test:τBar) stx-eq?))
    (check-false (ididmap-eq? test:FS (FS-set #'(c . baz) test:τBar) stx-eq?))))

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
