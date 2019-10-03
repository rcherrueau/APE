#lang racket/base

(require racket/match
         racket/function
         racket/list
         racket/pretty
         racket/syntax
         syntax/parse
         syntax/parse/define
         syntax/module-reader)

(provide (all-defined-out))

(define-literal-set keyword-lits
  ;; Note: I have to define new, send, ... as datum otherwise they
  ;; are going to be interpreted as identifier during macro
  ;; expansion and may risk an evaluation with new, send from
  ;; racket/class.
  #:datum-literals (new send get-field set-field! this)
  ;; I have no literals that should be interpreted.
  ())

(define-literal-set expr-lits
  #:datum-literals (prog class field def let)
  ())

(define-literal-set arg-lits
  #:datum-literals (: →)
  ())

(define-literal-set *keyword-lits
  #:datum-literals (*new *send *get-field *set-field! *this)
  ())

(define-literal-set *expr-lits
  #:datum-literals (*prog *class *field *def *let)
  ())


;; -- Syntax class for type and arg
(define-syntax-class type
  #:description "class' type with ownership and context parameters"
  #:attributes [TYPE OWNER CPARAMS]
  #:datum-literals (/)  ;; Don't consider '/' as a pattern
  (pattern (O:id / T:id)
           #:with OWNER #'O
           #:with TYPE #'T
           #:with CPARAMS #'())
  (pattern (O:id / (T:id PARAMS:id ...+))
           #:with OWNER #'O
           #:with TYPE #'T
           #:with CPARAMS #'(PARAMS ...))
  (pattern T:id
           #:with OWNER #''Θ
           #:with TYPE #'T
           #:with CPARAMS #''())
  (pattern (T:id PARAMS:id ...+)
           #:with OWNER #''Θ
           #:with TYPE #'T
           #:with CPARAMS #'(PARAMS ...))
  )

(define-syntax-class arg
  #:description "argument with its type"
  #:literal-sets [arg-lits]
  (pattern (NAME:id : T:type)
           #:attr OWNER #'T.OWNER
           #:attr TYPE  #'T.TYPE
           #:attr CPARAMS #'T.CPARAMS))

;; (define (arg->ownership-scheme stx)
;;   (syntax-parse stx
;;     [A:arg (type->ownership-scheme #'A.T)]
;;     [raise-syntax-error #f "Not an ownership argument" stx])
;;   )

;; (define (arg->ownership-scheme stx)
;;   (syntax-parse stx
;;     [ARG:arg #'()])
;;   )

;; (get-arg-name #'(a : b))
(define (get-arg-name stx)
  (syntax-parse stx
    [ARG:arg #'ARG.NAME]
    [ARG-NAME:id this-syntax]))


(define current-class-type (make-parameter #f))
(define local-bindings     (make-parameter #hash{}))

(define (is-locally-binded? stx)
  ;; (writeln (local-bindings))
  (let ([id-name (syntax->datum stx)])
    (hash-has-key? (local-bindings) id-name)))

(define (bind-ref stx)
  (let ([id-name (syntax->datum stx)])
    (hash-ref (local-bindings) id-name)))
