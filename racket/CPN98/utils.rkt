;; #lang typed/racket/base/no-check
#lang racket/base

(require (for-syntax racket/base
                     racket/exn
                     racket/pretty
                     racket/port
                     racket/string
                     racket/sequence
                     racket/syntax
                     syntax/srcloc)
         racket/dict
         racket/exn
         racket/function
         racket/list
         racket/match
         racket/port
         racket/pretty
         racket/sequence
         racket/string
         racket/syntax
         rackunit
         syntax/parse
         syntax/parse/define
         syntax/macro-testing
         syntax/srcloc
         syntax/stx
         errortrace/errortrace-key
         )

(provide (except-out (all-defined-out)
                     private:check-stx=?-w/-msg))

(define-for-syntax (stx->string stx)
  (call-with-output-string
   (λ (out-str)
     (pretty-print (syntax->datum stx)
                   out-str
                   #:newline? #f))))

(define-logger sclang)


(define ∘ compose1)

;; (: *** (All (a b c d) ((a -> b) (c -> d) -> ((Pairof a c) -> (Pairof b d)))))
;; map over a tuple (haskell bimap)
;;
;; (map (integer? . *** . string?)
;;      '((1 . "one") (2 . "two") (3.14 . #\π)))
;; > '((#t . #t) (#t . #t) (#f . #f))
(define (*** f g)
  (λ (ac)
    (let ([a (car ac)]
          [c (cdr ac)])
      (cons (f a) (g c)))))

;; (zip '(1 2 3) '(a b c))
;; > '((1 . a) (2 . b) (3 . c))
;; (: zip (All (a b) ((Listof a) (Listof b) -> (Listof (Pairof a b)))))
(define (zip l1 l2) (map cons l1 l2))

;; (unzip '((1 . a) (2 . b) (3 . c)))
;; > '(1 2 3)
;; > '(a b c)
;; (: unzip (All (a b) ((Listof (Pairof a b)) -> (Values (Listof a) (Listof b)))))
(define (unzip l)
  ;; (struct (X Y) 2-tuple ([fst : X] [snd : Y])
  (struct 2-tuple (fst snd)
    #:constructor-name mk-2-tuple)

  ;; (: unzip-l (2-tuple (Listof a) (Listof b)))
  (define unzip-l
    (foldr (λ (#;[v : (Pairof a b)]
               #;[as-bs : (2-tuple (Listof a) (Listof b))]
               v as-bs)
           (let ([as (2-tuple-fst as-bs)]
                 [bs (2-tuple-snd as-bs)])
             (mk-2-tuple (cons (car v) as)
                         (cons (cdr v) bs))))
         (mk-2-tuple '() '())
         l))

  (values (2-tuple-fst unzip-l) (2-tuple-snd unzip-l)))

;; (string-contains-once? "lala+lala"  #\+)  ; #t
;; (string-contains-once? "lala+lala+" #\+)  ; #f
;; (: string-contains-once? (String Char -> Boolean))
(define (string-contains-once? str contained)
  (eq? (length (indexes-of (string->list str) contained)) 1))


;; mk-ow-type-surface
;;
;; Build the surface syntax of an ownership type
;;
;; (:mk-ow-type-surface (TYPE OWNER CPARAMS -> Syntax))
(define (mk-ow-type-surface type-stx owner-stx cparams-stx)
  (let* ([cparams-str
          (string-join (stx-map (∘ symbol->string syntax-e) cparams-stx))]
         [ow-type-surface
          (format-id owner-stx "~a/~a{~a}" owner-stx type-stx cparams-str
                     #:source type-stx)])
    (set-surface-stx ow-type-surface ow-type-surface)))

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
;; (: extract-exp-name ((All a) (Syntaxof a) -> (U Identifier #f)))
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

  (let ([elems-size (if (null? vs) 0 (length (car vs)))])
    (foldr vs->ls (build-list elems-size (const '())) vs)))


;; Equivalent of port->lines but uses `read-syntax` instead of
;; `read-lines`.
;;
;; (: port->lines-stx (String Input-Port -> Syntax))
(define (port->lines-stx source-name in)
  (define LINEs
    (port->list (curry read-syntax source-name) in))
  (datum->syntax #f LINEs))



;; cms->srclocs : continuation-marks -> (listof srcloc)
(define (cms->srclocs cms)
  (map
   (λ (x) (make-srcloc (list-ref x 1)
                       (list-ref x 2)
                       (list-ref x 3)
                       (list-ref x 4)
                       (list-ref x 5)))
   (continuation-mark-set->list cms errortrace-key)))

(define-check (private:check-stx=?-w/-msg stx1 stx2 msg)
  (with-check-info*
    (list (make-check-name 'check-stx=?)
          (make-check-location (build-source-location-list stx1))
          (make-check-actual stx1)
          (make-check-expected stx2)
          (make-check-message
           (or msg
               (format "#'~.a does not structurally equal to #'~.a"
                       (syntax->datum stx1)
                       (syntax->datum stx2)))))
    (thunk (check-equal? (syntax->datum stx1)
                         (syntax->datum stx2)))))

;; (check-stx=? #'foo #'foo)                               ; succeed
;; (check-stx=? #'foo #'bar)                               ; failed
;; (check-stx=? (syntax-parse #'foo [_:id #'bar])  #'bar)  ; suceed
;; (check-stx=? (syntax-parse #'foo [_:id #'bar])  #'baz)  ; faild #'bar != #'baz
(define (check-stx=? stx1 stx2 [msg #f])
  (private:check-stx=?-w/-msg stx1 stx2 msg))

;; Checks that `stx` raise a `exn-pred?` when parsed as `pattern`.
;;
;; > (check-ill-parsed (syntax-parse #'foo [_:str #t]))  ; succeed
;; > (check-ill-parsed (syntax-parse #'foo [_:id #t]))   ; failed
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
;; (check-ill-parsed (syntax-parse #'foo [_:id #t]) #:pred exn:fail?)  ; failed
;; (check-ill-parsed (syntax-parse #'foo [_:id #t]) #:msg "lala")      ; failed
(define-syntax check-ill-parsed
  (syntax-parser
    [(name:id stx:expr
        (~optional (~seq #:pred exn-pred?) #:defaults ([exn-pred? #'exn:fail:syntax?]))
        (~optional (~seq #:msg message) #:defaults ([message #'#f])))
     #`(test-case (format "Parse ~a as ~a" 'stx 'stx-parser)
         (with-check-info*
           (list (make-check-name 'name)
                 (make-check-location '#,(build-source-location-list #'stx))
                 (make-check-params (list 'stx 'stx-parser 'exn-pred?))
                 (make-check-message (or message (format "No ~a raised" 'exn-pred?))))
           (thunk
            (check-exn exn-pred? (thunk stx) message))))]))

;; Tests that a `stx' follows `pattern` and verifies `check`s.
;; `message` is optional
;;
;; > (test-pattern #'stx pat:id (check-stx=? #'stx #'pat))   ; succeed
;;
;; > (test-pattern #'stx pat:id (check-stx=? #'pat #'foo))   ; check failed
;; --------------------
;; Parse (syntax stx) as pat:id
;; ; FAILURE
;; ; /home/rfish/prog/APE/racket/CPN98/utils.rkt:292:25
;; name:       check-stx=?
;; location:   utils.rkt:292:25
;; actual:     #<syntax:stdin::282 stx>
;; expected:   #<syntax:stdin::314 foo>
;; --------------------
;;
;; > (test-pattern #'stx pat:str (check-stx=? #'stx #'pat))  ; parse failed
;; --------------------
;; Parse (syntax stx) as pat:str
;; ; FAILURE
;; ; /home/rfish/prog/APE/racket/CPN98/utils.rkt:300:12
;; name:       test-pattern
;; location:   utils.rkt:300:12
;; params:     '(#'stx pat:str)
;; message:    "Failed to parse syntax as pat:str"
;; --------------------
(define-syntax test-pattern
  (syntax-parser
    [(name:id stx:expr pattern:expr check:expr ...+
              (~optional (~seq #:msg message)))
     ;; #`(test-case (format "Parse ~a as ~a" 'stx 'pattern)
     #`(with-handlers ([exn:fail:syntax?
                        (λ (e)
                          (with-check-info*
                            (list (make-check-name 'name)
                                  (make-check-location '#,(build-source-location-list #'stx))
                                  ;; Note: The `~?` inserts the `(make-check-message
                                  ;; message)` if `#:msg` is defined.  Or does nothing
                                  ;; otherwise (i.e., `(~@)`).  See,
                                  ;; https://docs.racket-lang.org/syntax/Optional_Keyword_Arguments.html#%28part._.Optional_.Arguments_with___%29
                                  (~? (make-check-message message) (~@)))
                            (thunk (check-not-exn (thunk (raise e))))))])
          (syntax-parse stx
            [pattern check ...]))]))

;; Equivalent to (for/and ([i (syntax->list STXL)]) BODY ...).
(define-syntax (stx-for/and stx)
  (syntax-case stx ()
    [(_ ([FOR-CLAUSE-ID FOR-CLAUSE-STXL] ...) BODY ...)
     #'(for/and ([FOR-CLAUSE-ID (in-syntax FOR-CLAUSE-STXL)] ...)
         BODY ...)]))

(define (stx-flatten stxls)
  #;(for ([stxl (in-list stxls)]
        [i (in-naturals)])
    (unless (stx-list? stxl)
      (apply raise-type-error 'stx-flatten "stx-list" i stxls)))
  (define flatten-stx (flatten (map syntax->list stxls)))
  #`#,flatten-stx
  )


;; (: set-surface-stx (All (a) (Syntaxof a) Syntax -> (Syntaxof a)))
(define (set-surface-stx stx surface-stx)
  ;; Reuse 'surface property of `surface-stx' or make `surface-stx' a
  ;; property.
  (define surface-prop (or (syntax-property surface-stx 'surface) surface-stx))

  ;; Mark `stx` with its surface
  (syntax-property stx 'surface surface-prop #t))

;; (: set-surface-stx (Syntax -> Syntax))
(define (get-surface-stx stx)
  (or (syntax-property stx 'surface) stx))

;; Add or preserve the 'surface property of the `origin' syntax object
;; to `stx'.
;;
;; (syntax-property (stx/surface #'bar #'foo) 'surface)
;; > #<syntax:stdin::233 foo>
;; (syntax-property (stx/surface #'baz (stx/surface #'bar #'foo)) 'surface)
;; > #<syntax:stdin::306 foo>
(define-syntax-rule (stx/surface stx surface-stx)
  ;; Set source location of `stx' by the one of `surface-stx'
  (let ([stx/loc (syntax/loc surface-stx stx)])
    ;; Set the surface property
    (set-surface-stx stx/loc surface-stx)))

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
         (log-sclang-debug $dbg-msg 'srcloc 'ctx res)
         res)]
    [(_ E) #'(dbg E #:ctx E)]))
