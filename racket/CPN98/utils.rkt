#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/define
                     racket/syntax
                     racket/match
                     "definitions.rkt"
                     )
         syntax/parse
         syntax/parse/define
         racket/match
         racket/list
         racket/syntax
         "definitions.rkt"
         )

(provide (all-defined-out))



(define ∘ compose1)

;; -- Syntax checker in the form of
;; https://docs.racket-lang.org/syntax/syntax-helpers.html#%28part._stxkeyword%29
;; A check procedure consumes the syntax to check and a context
;; syntax object for error reporting and either raises an error to
;; reject the syntax or returns a value as its parsed
;; representation.
;;
;; Returns the first duplicate class in the program or #f if there
;; are no duplicate.
(define (check-class clss-stx)
  (define (get-class-name cls-stx)
    (syntax-case cls-stx (class)
      [(class name)       #'name]
      [(class name _ ...) #'name]
      [_ (raise-syntax-error #f "Bad syntax for class" cls-stx)]))

  (let* ([classes     (syntax->list clss-stx)]
         [class-names (map get-class-name classes)])
    (check-duplicate-identifier class-names)))

;; Returns the first duplicate field in the class or #f if there are
;; no duplicate.
(define (field-twice? cls-stx)
  (define (get-field-name field/def-stx)
    (syntax-case field/def-stx (field :)
      ;; (field [NAME : TYPE])
      [(field [name : _]) #'name]
      [_                  #f]))

  (let* ([fields/defs (syntax->list cls-stx)]
         [field-names (filter-map get-field-name fields/defs)])
    (check-duplicate-identifier field-names)))

;; Returns the first duplicate def in the class or #f if there are
;; no duplicate.
(define (def-twice? cls-stx)
  (define (get-def-name field/def-stx)
    (syntax-case field/def-stx (def : →)
      ;; (def (NAME [ARG:NAME : ARG:TYPE] ... → RET:TYPE) E)
      [(def (name _ ...) _) #'name]
      [_                      #f]))

  (let* ([fields/defs (syntax->list cls-stx)]
         [def-names   (filter-map get-def-name fields/defs)])
    (check-duplicate-identifier def-names)))


;; (syntax-property* #'() 'p1 'v1 'p2 'v2 'p3 'v3)
(define-syntax-parser syntax-property*
  [(_ stx)
   #'stx]
  [(_ stx key v ...+)
   #:with [v-hd kv-tl ...] #'(v ...)
   #'(syntax-property (syntax-property* stx kv-tl ...) key v-hd)])

;; c-type> :: (C-TYPE: stx) -> (t: stx -> stx) -> (stx -> stx)
;; Parameterize the syntax transformer `t` with class type `C-TYPE`.
(define (c-type> C-TYPE t)
  (λ (stx)
    (parameterize ([current-class-type C-TYPE]) (t stx))))

;; binder> :: (BINDER: stx) -> (t: stx -> stx) -> (stx -> stx)
(define (binder> BINDER t)
  (λ (stx)
    (let* ([binder-name  (syntax->datum (get-arg-name BINDER))]
           [new-bindings (hash-set (local-bindings) binder-name BINDER)])
      (parameterize ([local-bindings new-bindings]) (t stx)))))

;; binder*> :: (BINDERS: [stx]) -> (t: stx -> stx) -> (stx -> stx)
(define (binder*> BINDERS t)
  (match BINDERS
    ['()           t]
    [`(,b)         (binder> b t)]
    [`(,b ,bs ...) (binder> b (binder*> bs t))]))

;; put `foo in local-bindings
;; (+<binder #'foo (*d #'BODY))
;; ;; Expand to:
;; ;; (let* ([arg-name-stxs (syntax-parse #'(ARG ...)
;; ;;                         [(A:arg ...) (syntax->list #'(A.NAME ...))])]
;; ;;        [arg-names (map syntax->datum arg-name-stxs)]
;; ;;        [arg-stxs (syntax->list #'(ARG ...))]
;; ;;        [binders (interleave arg-names arg-stxs)]
;; ;;        [new-bindings (apply hash-set* (local-bindings) binders)])
;; ;;  (parameterize ([local-bindings new-bindings]) (*d #'BODY))))
;; (define-syntax-parser binder>
;;   [(_ BINDERS BODY)
;;    ;; `bindings` Dynamic scoping for local-bindings
;;    ;; TODO: use syntax-parameter
;;    #:with bindings (datum->syntax #'BINDERS 'local-bindings)
;;    #'(let* ([binder-stxs  ;; Put the BINDERS in a list
;;              ;; BINDERS could be a list of id, such as in def:
;;              ;;   (+<bind #'(ARG ...))
;;              ;; Or a simple id, such as in let:
;;              ;;   (+<bind #'ARG)
;;              ;; The `syntax->list' returns #f is the second case.
;;              (match (syntax->list BINDERS) [#f (list BINDERS)] [bs bs])]
;;             [binder-name-stxs (map get-arg-name binder-stxs)]
;;             [binder-names (map syntax->datum binder-name-stxs)]
;;             [binders (interleave binder-names binder-stxs)]
;;             [new-bindings (apply hash-set* (bindings) binders)])
;;        (parameterize ([bindings new-bindings]) BODY))])

;; (define (bind binding-table binder body)
;;    ;; Dynamic scoping for local-binding-table
;;    ;; #:with binding-table (datum->syntax #'BINDER 'local-binding)
;;    ;; Put BINDER in the local binding table and parameterize the BODY
;;    ;; with the new local-binding-table
;;   (let* ([binder-name  (syntax->datum binder)]
;;          [new-binding-table (hash-set (binding-table) binder-name binder)])
;;     (parameterize ([binding-table new-binding-table]) (body))))

;; (define (bind* name binders body)
;;   (match binders
;;     ['(x) (bind x (body))])

;;   (writeln name)
;;   (writeln binders)
;;   (body)

;;   )
  ;; [(_ (BINDER) BODY)
  ;;  #'(bind BINDER BODY)]
  ;; [(_ (B1 BS ...) BODY)
  ;;  (writeln #'B1)
  ;;  (writeln #'(BS ...))
  ;;  #'(bind (B1) (bind* (BS ...) BODY))
  ;;  ])

;; (interleave '(1 2 3) '(a b c))
(define (interleave xs ys)
  (match (list xs ys)
    [`((,x ,xs ...) (,y ,ys ...)) (cons x (cons y (interleave xs ys)))]
    [else '()])
  )

(define (c-type+ stx)
  (syntax-property stx 'class (current-class-type)))

;; (define-syntax-parser c-type+
;;   [(_ STX)
;;    ;; `class-type` Dynamic scoping for current-class-type
;;    ;; TODO: use syntax-parameter
;;    #:with class-type (datum->syntax #'STX 'current-class-type)
;;    #'(syntax-property STX 'class (class-type))])

(define (binder+ stx)
  ;; (printf "-- ~s binded by ~s~n" stx (bind-ref stx))
  (syntax-property stx 'binder (bind-ref stx)))

;; (define-syntax-parser binder+
;;   [(_ STX)
;;    ;; `bind-ref` Dynamic scoping for `bind-ref
;;    ;; TODO: use syntax-parameter
;;    #:with bind-ref   (datum->syntax #'STX 'bind-ref)
;;    #'(syntax-property STX 'binder (bind-ref STX))]
;;   )

;; (define-syntax-parser @bind/class
;;   [(_ STX)
;;    #'(@bind (@class STX))])
