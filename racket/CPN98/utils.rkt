#lang typed/racket/base/no-check

(require (for-syntax racket/base
                     racket/pretty
                     racket/port
                     syntax/srcloc)
         racket/dict
         racket/exn
         racket/function
         racket/list
         racket/match
         racket/string
         rackunit
         syntax/parse
         syntax/parse/define
         errortrace/errortrace-key
         )

(provide (all-defined-out))


(define ∘ compose1)

;; (zip '(1 2 3) '(a b c))
;; > '((1 . a) (2 . b) (3 . c))
(: zip (All (a b) ((Listof a) (Listof b) -> (Listof (Pairof a b)))))
(define (zip l1 l2) (map cons l1 l2))

;; (unzip '((1 . a) (2 . b) (3 . c)))
;; > '(1 2 3)
;; > '(a b c)
(: unzip (All (a b) ((Listof (Pairof a b)) -> (Values (Listof a) (Listof b)))))
(define (unzip l)
  (struct (X Y) 2-tuple ([fst : X] [snd : Y])
    #:constructor-name mk-2-tuple)

  (: unzip-l (2-tuple (Listof a) (Listof b)))
  (define unzip-l
    (foldr (λ ([v : (Pairof a b)]
             [as-bs : (2-tuple (Listof a) (Listof b))])
           (let ([as (2-tuple-fst as-bs)]
                 [bs (2-tuple-snd as-bs)])
             (mk-2-tuple (cons (car v) as)
                         (cons (cdr v) bs))))
         (mk-2-tuple '() '())
         l))

  (values (2-tuple-fst unzip-l) (2-tuple-snd unzip-l)))

;; (string-contains-once? "lala+lala"  #\+)   ; #t
;; (string-contains-once? "lala+lala+" #\+)  ; #f
(: string-contains-once? (String Char -> Boolean))
(define (string-contains-once? str contained)
  (eq? (length (indexes-of (string->list str) contained)) 1))

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


;; Returns `#t` if the syntax object is a field.
;; field? : Syntax -> Boolean
(define field?
  (syntax-parser
    #:datum-literals (field)
    [(field x ...) #t]
    [_ #f]))

;; Returns `#t` if the syntax object is a def.
;; def? : Syntax -> Boolean
(define def?
  (syntax-parser
    #:datum-literals (def)
    [(def _ ...) #t]
    [_ #f]))

;; From https://github.com/LiberalArtist/adjutor/blob/9d1bb66d2cb4751c72d8a8da4f11b982b282128c/kernel.rkt#L68
(define-syntax values->list
  (syntax-parser
    [(_ body:expr ...+)
     #'(call-with-values (λ () body ...) list)]))

(define (list->values lst)
  (apply values lst))

;; > (sequence '((a 1 "foo") (b 2 "bar") (c 3 "baz")))
;; '((a b c) (1 2 3) ("foo" "bar" "baz"))
(define (sequence vs)
  (define (vs->ls v ls)
    (map (λ (a b) (cons a b)) v ls))
  (foldr vs->ls (build-list (length (car vs)) (const '())) vs))


#;(define-custom-hash-types priv:idid-map
  #:key? (λ ([K : Idid])
           (if (syntax? K)
               (let ([k (syntax-e K)])
                 (and (pair? k) (identifier? (car k)) (identifier? (cdr k))))
               #f))
  (λ ([X : Idid] [Y : Idid])
    (let* ([x (syntax-e X)]
           [y (syntax-e Y)]
           [x1 (car x)] [x2 (cdr x)]
           [y1 (car y)] [y2 (cdr y)])
      (if (and (eq? (syntax-e x1) (syntax-e y1))
               (eq? (syntax-e x2) (syntax-e y2)))
          #t #f)))
  )


;; cms->srclocs : continuation-marks -> (listof srcloc)
(define (cms->srclocs cms)
  (map
   (λ (x) (make-srcloc (list-ref x 1)
                       (list-ref x 2)
                       (list-ref x 3)
                       (list-ref x 4)
                       (list-ref x 5)))
   (continuation-mark-set->list cms errortrace-key)))

(define-simple-check (check-stx=? stx1 stx2)
  (equal? (syntax->datum stx1) (syntax->datum stx2)))


;; (dbg (+ 1 2))
;; > ; [dbg] utils.rkt:149:5: '(+ 1 2) = 3
;; > 3
(define-syntax (dbg stx)
  (syntax-case stx ()
    [(_ E)
     #`(let
           ([srcloc #,(srcloc->string (build-source-location #'E))]
            [datum  #,(call-with-output-string
                       (λ (out-str) (pretty-print (syntax->datum #'E)
                                                  out-str
                                                  #:newline? #f)))]
            [res         E]
            [$dbg-msg    "; [dbg] ~a: ~a = ~s"])
         (log-fatal $dbg-msg srcloc datum res)
         res)]))
