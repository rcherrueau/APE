#lang racket/base

(require (for-syntax racket/base
                     racket/pretty
                     racket/port)
         racket/exn
         racket/match
         racket/string
         syntax/parse
         syntax/parse/define
         errortrace/errortrace-key
         "definitions.rkt")

(provide (all-defined-out))


(define ∘ compose1)

;; (zip '(1 2 3) '(a b c))
;; > '((1 . a) (2 . b) (3 . c))
(define (zip l1 l2)
  (for/list ([i l1] [j l2]) (cons i j)))

;; (unzip '((1 . a) (2 . b) (3 . c)))
;; > '(1 2 3)
;; > '(a b c)
(define (unzip l)
  (for/lists (firsts seconds #:result (values firsts seconds))
             ([pr l])
    (values (car pr) (cdr pr))))

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

(define-simple-macro (define-parser ID:id RHS ...)
  (define ID (syntax-parser RHS ...)))



;; cms->srclocs : continuation-marks -> (listof srcloc)
(define (cms->srclocs cms)
  (map
   (λ (x) (make-srcloc (list-ref x 1)
                       (list-ref x 2)
                       (list-ref x 3)
                       (list-ref x 4)
                       (list-ref x 5)))
   (continuation-mark-set->list cms errortrace-key)))


;; (dbg (+ 1 2))
(define-syntax-parser dbg
  [(_ E:expr)
   #`(let*-values
         ([(src)         #,(syntax-source #'E)]
          [(base file _) (split-path src)]
          [(line)        #,(syntax-line #'E)]
          [(col)         #,(syntax-column #'E)]
          [(datum)       #,(call-with-output-string
                            (λ (out-str) (pretty-print (syntax->datum #'E)
                                                       out-str
                                                       #:newline? #f)))]
          [(res)         E]
          [($dbg-msg)    "; [dbg] ~a:~s:~s: ~a = ~s"])
       (log-fatal $dbg-msg file line col datum res)
       res)])
