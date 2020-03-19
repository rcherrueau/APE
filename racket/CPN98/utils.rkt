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

(define-for-syntax (stx->string stx)
  (call-with-output-string
   (λ (out-str)
     (pretty-print (syntax->datum stx)
                   out-str
                   #:newline? #f))))

(define-logger lang)


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

;; Tests that a `stx' follows `pattern` and verifies `check`s.
;; `message` is optional
;;
;; > (test-parse #'stx pat:id (check-stx=? #'stx #'pat))   ; succeed
;;
;; > (test-parse #'stx pat:id (check-stx=? #'pat #'foo))   ; check failed
;; --------------------
;; Parse (syntax stx) as pat:id
;; ; FAILURE
;; ; /home/rfish/prog/APE/racket/CPN98/utils.rkt:292:25
;; name:       check-stx=?
;; location:   utils.rkt:292:25
;; params:
;; '(#<syntax:/home/rfish/prog/APE/racket/CPN98/utils.rkt:292:14 stx>
;;   #<syntax:/home/rfish/prog/APE/racket/CPN98/utils.rkt:292:46 foo>)
;; --------------------
;;
;; > (test-parse #'stx pat:str (check-stx=? #'stx #'pat))  ; parse failed
;; --------------------
;; Parse (syntax stx) as pat:str
;; ; FAILURE
;; ; /home/rfish/prog/APE/racket/CPN98/utils.rkt:300:12
;; name:       test-parse
;; location:   utils.rkt:300:12
;; params:     '(#'stx pat:str)
;; message:    "Failed to parse syntax as pat:str"
;; --------------------
(define-syntax test-parse
  (syntax-parser
    [(name:id stx:expr pattern:expr check:expr ...+
              (~optional (~seq #:msg message) #:defaults ([message #'#f])))
     #`(test-case (format "Parse ~a as ~a" 'stx 'pattern)
         (syntax-parse stx
           [pattern check ...]
           [_
            (with-check-info*
              (list (make-check-name 'name)
                    (make-check-location '#,(build-source-location-list #'stx))
                    (make-check-params (list 'stx 'pattern))
                    (make-check-message
                     (or message
                         (format "Failed to parse syntax as ~a" 'pattern))))
               fail-check)]))]))

;; Checks that `stx` raise a `exn-pred?` when parsed as `pattern`.
;;
;; > (check-ill-parsed #'foo _:str)  ; succeed
;; > (check-ill-parsed #'foo _:id)   ; failed
;; --------------------
;; Parse (syntax foo) as _:id
;; ; FAILURE
;; ; /home/rfish/prog/APE/racket/CPN98/utils.rkt:325:17
;; name:       check-ill-parsed
;; location:   utils.rkt:325:17
;; params:     '(#'foo _:id exn:fail:syntax?)
;; message:    "No exception raised"
;; --------------------
;;
;; Other usages:
;; (check-ill-parsed #'foo _:id #:pred exn:fail?)  ; failed
;; (check-ill-parsed #'foo _:id #:msg "lala")      ; failed
(define-syntax check-ill-parsed
  (syntax-parser
    [(name:id stx:expr pattern:expr
        (~optional (~seq #:pred exn-pred?) #:defaults ([exn-pred? #'exn:fail:syntax?]))
        (~optional (~seq #:msg message) #:defaults ([message #'#f])))
     #`(test-case (format "Parse ~a as ~a" 'stx 'pattern)
         (with-check-info*
           (list (make-check-name 'name)
                 (make-check-location '#,(build-source-location-list #'stx))
                 (make-check-params (list 'stx 'pattern 'exn-pred?))
                 (make-check-message (or message (format "No ~a raised" 'exn-pred?))))
           (thunk
            (check-exn exn-pred?
                       (thunk (syntax-parse stx [pattern #t]))
                       message))))]))

;; Equivalent to (for/and ([i (syntax->list STXL)]) BODY ...).
(define-syntax (stx-for/and stx)
  (syntax-case stx ()
    [(_ ([FOR-CLAUSE-ID FOR-CLAUSE-STXL] ...) BODY ...)
     #'(for/and ([FOR-CLAUSE-ID (syntax->list FOR-CLAUSE-STXL)] ...)
         BODY ...)]))



;; Add or preserve the 'surface property of the `origin' syntax object
;; to `stx'.
;;
;; (syntax-property (stx/surface #'bar #'foo) 'surface)
;; > #<syntax:stdin::233 foo>
;; (syntax-property (stx/surface #'baz (stx/surface #'bar #'foo)) 'surface)
;; > #<syntax:stdin::306 foo>
(define-syntax-rule (stx/surface stx origin)
  (let* (;; Set source location of `stx' by the one of `surface'
         [stx/loc (syntax/loc origin stx)]
         ;; Reuse 'surface property of `surface' or make `surface' the
         ;; property
         [surface-prop (or (syntax-property origin 'surface) origin)]
         [new-stx (syntax-property stx/loc 'surface surface-prop #t)])
    ;; (println surface-prop)
    ;; (println (syntax-property-symbol-keys new-stx))
    new-stx))

;; (dbg (+ 1 2))
;; > ; [dbg] stdin::103: (+ 1 2) = 3
;; > 3
;;
;; (dbg (+ 1 2) #:ctx add)
;; > ; [dbg] stdin::107: add = 3
;; > 3
(define-syntax (dbg stx)
  (syntax-parse stx
    [(_ E #:ctx ctx)
     ;; Note on "?: literal data is not allowed;" error.
     ;;
     ;; A literal data in Racket is anything that isn't a symbol or a
     ;; pair. And, a literal data, such as the following `srcloc`, is
     ;; not legal in a fully-expanded program. It is only legal in a
     ;; fully-expanded program when wrapped in `quote` or
     ;; `quote-syntax`. I can fix this by explicitly wrap it in a
     ;; quote.
     ;;
     ;; https://groups.google.com/d/msg/racket-users/HaSmcTN0SA4/IM6MR_TlAgAJ
     #:with srcloc (srcloc->string (build-source-location #'ctx))
     #'(let ([$dbg-msg "; [dbg] ~a: ~a = ~s~n"]
             [res      E])
         (log-lang-error $dbg-msg 'srcloc 'ctx res)
         res)]
    [(_ E) #'(dbg E #:ctx E)]))
