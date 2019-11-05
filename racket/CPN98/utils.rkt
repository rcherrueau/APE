#lang racket/base

(require (for-syntax racket/base
                     racket/pretty
                     racket/port)
         racket/match
         syntax/parse
         syntax/parse/define
         "definitions.rkt")

(provide (all-defined-out))


(define ∘ compose1)

;; (syntax-property-symbol-keys
;;   (syntax-property* #'() 'p1 'v1 'p2 'v2 'p3 'v3))
;;
;; (syntax-property (syntax-property* #'() 'p1 'v1 'p2 'v2 'p3 'v3) 'p1) ;; 'v1
;; (syntax-property (syntax-property* #'() 'p1 'v1 'p2 'v2 'p3 'v3) 'p2) ;; 'v2
;; (syntax-property (syntax-property* #'() 'p1 'v1 'p2 'v2 'p3 'v3) 'p3) ;; 'v3
(define-syntax-parser syntax-property*
  [(_ stx)
   #'stx]
  [(_ stx key val kvs ...)
   #'(syntax-property (syntax-property* stx kvs ...) key val)])


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

;; ;; (interleave '(1 2 3) '(a b c))
;; (define (interleave xs ys)
;;   (match (list xs ys)
;;     [`((,x ,xs ...) (,y ,ys ...)) (cons x (cons y (interleave xs ys)))]
;;     [else '()])
;;   )

(define current-class-type (make-parameter #f))
(define local-bindings     (make-parameter #hash{}))

;; Set the class type of a syntax object
(define (c-type+ stx)
  ;; (printf "// ~s comes from class ~s~n" stx (current-class-type))
  (c-type-prop stx (current-class-type)))

(define (bind-ref stx)
  (let ([id-name (syntax->datum stx)])
    (hash-ref (local-bindings) id-name)))

;; Set the binder of a syntax object
(define (binder+ stx)
  ;; (printf "-- ~s binded by ~s~n" stx (bind-ref stx))
  (binder-prop stx (bind-ref stx)))

;; Test if a syntax object is binder.
(define (binded? stx)
  (binder-prop stx))

(define-simple-macro (define-parser ID:id RHS ...)
  (define ID (syntax-parser RHS ...)))

;; (dbg (+ 1 2))
(define-syntax-parser dbg
  [(_ E:expr)
   #`(let ([src    #,(syntax-source #'E)]
           [line   #,(syntax-line #'E)]
           [col    #,(syntax-column #'E)]
           [datum  #,(call-with-output-string
                      (λ (out-str) (pretty-print (syntax->datum #'E) out-str)))]
           [res    E])
       (define-values (base file _) (split-path src))
       (printf "[~a:~s:~s] ~a = ~s~n" file line col datum res)
       res)])
