#lang racket/base

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
         ;; "definitions.rkt"
         )

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


;; (define-type IdId (Syntaxof (Pairof Identifier Identifier)))
;; (define-type (IdIdMap V) (Listof (Pairof IdId V)))


;; (:idid? (IdId -> Boolean))
(define (idid? X)
  (if (syntax? X)
      (let ([x (syntax-e X)])
        (and (pair? x)
             (identifier? (car x))
             (identifier? (cdr x)) #t))
      #f))

;; (: idid-eq? (IdId IdId -> Boolean))
(define (idid-eq? X Y)
  (let* ([x (syntax-e X)]
         [y (syntax-e Y)]
         [x1 (car x)] [x2 (cdr x)]
         [y1 (car y)] [y2 (cdr y)])
    (if (and (eq? (syntax-e x1) (syntax-e y1))
             (eq? (syntax-e x2) (syntax-e y2)))
        #t #f)))

;; (: ididmap-ref (IdIdMap IdId -> Identifier))
(define (ididmap-ref ididmap key)
  (cond
    [(assoc key ididmap idid-eq?) => cdr]
    [else (error "the key ~s does not exist in IdIdMap" key)]))

;; (: ididmap-set
;;    (IdIdMap IdId Identifier -> IdIdMap))
(define (ididmap-set ididmap key val)
  (cond
    [(private:ididmap-index-of ididmap key)
     => (λ (idx) (list-set ididmap idx (cons key val)))]
    [else (cons (cons key val) ididmap)]))

(define (private:ididmap-index-of ididmap key)
  (let-values ([(keys _) (unzip ididmap)])
    (index-of keys key idid-eq?)))

;; (: ididmap-has-key?
;;    (IdIdMap IdId -> Boolean))
(define (ididmap-has-key? ididmap key)
  (and (private:ididmap-index-of ididmap key) #t))

(define (ididmap-eq? map1 map2 [v-eq? eq?])
  (for/and ([kv1 map1]
            [kv2 map2])
    (let ([k1 (car kv1)] [v1 (cdr kv1)]
          [k2 (car kv2)] [v2 (cdr kv2)])
      (and (idid-eq? k1 k2) (v-eq? v1 v2)))))



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
