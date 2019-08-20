#lang racket

(require (for-syntax syntax/parse
                     racket/syntax)
         syntax/parse
         syntax/parse/define)

(provide (rename-out [@%module-begin #%module-begin]
                     [@%top #%top] ;; wrapped unbound variables
                     [@let let]
                     [@%datum #%datum]
                     )
         class field def
         new
         get-field set-field!
         send
         #%app #%top-interaction
         )

;; ;; (define-syntaxclass-parser C : class
;; ;;   #'(foo C.NAME [C.OWNER ...+] C.FIELD ...))
;; ;; >
;; ;; > (define-syntax-parser class
;; ;; >   [(C:class)
;; ;; >    #'(foo C.NAME (C.OWNER ...+) C.FIELD ...)])
;; (define-syntax (define-syntaxclass-parser stx)
;;   (syntax-parse stx
;;     [(_ PAT:id (~datum :) STX-CLASS:id CLAUSE:expr)
;;      (let* ([pat       (syntax->datum #'PAT)]
;;             [stx-class (syntax->datum #'STX-CLASS)]
;;             [PAT:CLASS (format-id stx "~a:~a" pat stx-class)])
;;        #`(define-syntax-parser STX-CLASS
;;            [(#,PAT:CLASS) CLAUSE]))]))

(begin-for-syntax
  (define-syntax-class type
    (pattern TYPE:id))

  (define-syntax-class arg
    (pattern (NAME:id (~datum :) TYPE:type))))

(define-for-syntax (forall p l)
  (foldr eq? #t (map p l)))

(define-for-syntax (is-syntax-value? value stx)
  (define expr (syntax-e (car (syntax->list stx))))
  (eq? value expr))

(define-for-syntax (is-class? stx)
  (is-syntax-value? 'class stx))

(define-for-syntax (is-field? stx)
  (is-syntax-value? 'field stx))

(define-for-syntax (is-def? stx)
  (is-syntax-value? 'def stx))

(define-for-syntax (is-field/def? stx)
  (or (is-field? stx) (is-def? stx)))

(define-for-syntax (is-expr? stx)
  (define expr (syntax-e stx))
  (displayln expr)
  (cond
    [(symbol? expr) #t]  ;; variable
    [(number? expr) #t]
    [else
     (let ([expr (syntax-e (car expr))])
       (displayln expr)
       (member expr '(new let send get-field set-field!)))]))

(define-syntax-parser @%module-begin
  [(_ CLASS:expr ... E:expr)
   ;; Check `CLASS ...` is a list of class definition
   #:when (forall is-class? (syntax->list #'(CLASS ...)))
   ;; Check `E` is an expression
   #:when (is-expr? #'E)
   #'(#%module-begin CLASS ... E)])


;; ----- Class
(define-syntax-parser class
  [(_ NAME:id [OWNER:id ...+] BODY:expr ...)
   ;; Check `BODY ...` is made of field and def
   #:when (forall is-field/def? (syntax->list #'(BODY ...)))
   #'`(Class NAME [OWNER ...] ,BODY ...)])

(define-syntax-parser field
  [(_ ARG:arg)
   #'`(Field ,ARG.NAME : ARG.TYPE)])

(define-syntax-parser def
  [(_ (DNAME ARG:arg ...) BODY:expr)
   ;; Check `BODY` is an expression
   #:when (is-expr? #'BODY)
   #'`(Def (DNAME [,ARG.NAME : ARG.TYPE] ... ) ,BODY)])

;; ---- Expression
(define-syntax-parser new
  [(_ CNAME:id)
   #''(New CNAME)])

(define-syntax-parser @let
  [(_ (ARG-NAME:id (~datum :) ARG-TYPE:type E:expr) BODY:expr)
   ;; Check `E` is an expression
   #:when (is-expr? #'E)
   ;; Check `BODY` is an expression
   #:when (is-expr? #'BODY)
   #'`(Let (,ARG-NAME : ARG-TYPE ,E) ,BODY)])

(define-syntax-parser get-field
  [(_ CNAME:id FNAME:id)
   #'`(Get-field ,CNAME FNAME)])

(define-syntax-parser set-field!
  [(_ CNAME:id FNAME:id E:expr)
   ;; Check `E` is an expression
   #:when (is-expr? #'E)
   #'`(Set-field! ,CNAME FNAME ,E)])

(define-syntax-parser send
  [(_ CNAME:id DNAME:id ARG:expr ...)
   #'`(Send ,CNAME DNAME ,ARG ...)])

;; ;; ---- Value
(define-syntax-parser @%datum
  [(_ . N:nat) #''(Num  N)])

(define-syntax-parser @%top
  [(_ . ID:id)
   ;; #''ID])
   #''(Var 'ID)])
