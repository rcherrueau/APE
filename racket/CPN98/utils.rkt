#lang typed/racket/base/no-check

(require (for-syntax racket/base
                     racket/pretty
                     racket/port
                     racket/string
                     racket/syntax
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

;; (string-contains-once? "lala+lala"  #\+)  ; #t
;; (string-contains-once? "lala+lala+" #\+)  ; #f
(: string-contains-once? (String Char -> Boolean))
(define (string-contains-once? str contained)
  (eq? (length (indexes-of (string->list str) contained)) 1))

;; Syntax to string
(define (stx->string stx #:newline? [newline? #t])
  (call-with-output-string
   (λ (out-str)
     (pretty-print (syntax->datum stx)
                   out-str
                   #:newline? newline?))))

;; Extract the name of a syntax expression.
;;
;; (extract-exp-name #'foo)               ; 'foo
;; (extract-exp-name #'(foo bar))         ; 'foo
;; (extract-exp-name #'(foo [bar baz]))   ; 'foo
;;
;; From https://github.com/racket/racket/blob/738d2b7a81aad981a03884d27a7d0429940eccaf/racket/src/expander/syntax/error.rkt#L105-L115
(: extract-exp-name ((All a) (Syntaxof a) -> (U Identifier #f)))
(define (extract-exp-name stx)
  (cond
    [(syntax? stx)
     (define e (syntax-e stx))
     (cond
       [(symbol? e) e]
       [(and (pair? e)
             (identifier? (car e)))
        (syntax-e (car e))]
       [else #f])]
    [else #f]))

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

;; ;; (interleave '(1 2 3) '(a b c))
;; (define (interleave xs ys)
;;   (match (list xs ys)
;;     [`((,x ,xs ...) (,y ,ys ...)) (cons x (cons y (interleave xs ys)))]
;;     [else '()])
;;   )

(define-syntax (set-box!-values stx)
  (syntax-parse stx
    [(_ (ID:id ...) E:expr)
     #:with [vID ...] (generate-temporaries #'(ID ...))
     #'(let-values ([(vID ...) E])
         (set-box! ID vID) ...
         )]))


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
(define-syntax (define-parser parser-stx)
  ;; Strip the `#:` from a (Syntaxof Keyword)
  (define (strip#: KEYWORD)
    (define keyword (keyword->string (syntax-e KEYWORD)))
    (datum->syntax KEYWORD (string->symbol keyword)))

  ;; Make the parser
  (syntax-parse parser-stx
    [(_ ID:id                          ;; Parser name (e.g., `?>`)
        #:literal-sets ls              ;; Literals
        (~seq K:keyword Env:expr) ...  ;; Environment variables
        CLAUSE ...+)                   ;; Clauses of the transfo
     #:with [def-K ...] (map strip#: (syntax->list #'(K ...)))
     #:with PARSER-ID         (generate-temporary)
     #'(begin
         ;; Define environment variables as global parameter
         (define def-K (make-parameter #f)) ...
         (define topCall (make-parameter #t))

         ;; Define parser as an internal def
         (define PARSER-ID
           (syntax-parser
             #:literal-sets ls
             CLAUSE ...))

         ;; Define the parser as a global definition
         (define (ID stx)

           ;; Parameterize the parser call with `Env` values at the
           ;; top Call (not recursive ones)
           (if (topCall)
               (parameterize ([def-K Env] ... [topCall #f]) (PARSER-ID stx))
               (PARSER-ID stx))))]))

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

;; Equivalent to (for/and ([i (syntax->list STXL)]) BODY ...).
(define-syntax (stx-for/and stx)
  (syntax-case stx ()
    [(_ ([FOR-CLAUSE-ID FOR-CLAUSE-STXL] ...) BODY ...)
     #'(for/and ([FOR-CLAUSE-ID (syntax->list FOR-CLAUSE-STXL)] ...)
         BODY ...)]))




;; (dbg (+ 1 2))
;; > ; [dbg] utils.rkt:149:5: '(+ 1 2) = 3
;; > 3
(define-syntax (dbg stx)
  (syntax-case stx ()
    [(_ E)
     #`(let
           ([srcloc #,(srcloc->string (build-source-location #'E))]
            [datum  #,(stx->string #'E)]
            [res         E]
            [$dbg-msg    "; [dbg] ~a: ~a = ~s"])
         (log-fatal $dbg-msg srcloc datum res)
         res)]))


(define-for-syntax (stx->string stx)
  (call-with-output-string
   (λ (out-str)
     (pretty-print (syntax->datum stx)
                   out-str
                   #:newline? #f))))
