#lang racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.
;;
;; Collect meta information for later (M>).
;;
;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects

(require racket/contract/base
         racket/function
         racket/list
         racket/match
         racket/syntax
         syntax/parse
         syntax/parse/define
         syntax/srcloc
         syntax/stx
         "utils.rkt"
         "definitions.rkt")

(provide M>)


(define (M> stx)
  ;; Get all class
  (define class-stxs
    (syntax-parse stx
      #:literal-sets [keyword-lits]
      [(prog ~! CLASS ... E) #'(CLASS ...)]))

  ;; Get all class' names, fields and defs
  (match-define (list ctype         ;; (listof name-stx)
                      ctype-fields  ;; (listof (pairof name-stx fields-stx))
                      ctype-defs)   ;; (listof (pairof name-stx defs-stx))
    (sequence (stx-map get-names/fields/defs class-stxs)))

  ;; Transforms ctype-fields to an FS
  (values
   ctype
   (foldr (λ (v l) (append (mk-fs (car v) (cdr v)) l) ) '() ctype-fields)
   (foldr (λ (v l) (append (mk-ds (car v) (cdr v)) l) ) '() ctype-defs))
  )


;; Get the fields indexed by the class name.
(define get-names/fields/defs
  (syntax-parser
    #:literal-sets [keyword-lits]
    [(class ~! NAME [CPARAM ...] FIELD/DEF ...)
     #:with [FIELD ...] (filter field? (stx->list #'(FIELD/DEF ...)))
     #:with [DEF ...] (filter def? (stx->list #'(FIELD/DEF ...)))
     (list #'NAME
           (cons #'NAME #'(FIELD ...))
           (cons #'NAME #'(DEF ...)))]))

;; (pairof name-stx (listof field-stx))
;; -> (listof (pairof (syntaxof (pairof name field-name)) field-type-stx))
(define (mk-fs C-TYPE FIELD...)
  (define mk-fs-item (syntax-parser
      #:literal-sets [keyword-lits]
      [(field NAME OWS:ow-scheme)
       #:with C-TYPE C-TYPE
       (cons #'(C-TYPE . NAME) #'OWS)]))

  (stx-map mk-fs-item FIELD...))


;; output:
;; (Syntaxof
;;  (List Identifier                     ; Class type
;;        Identifier                     ; Def name
;;        (Syntaxof (Listof OW-SCHEME))  ; Type of def args
;;        ))
(define (mk-ds C-TYPE DEF...)
  (define mk-ds-item (syntax-parser
      #:literal-sets [keyword-lits]
      [(def (NAME (A-NAME A-OWS:ow-scheme) ...  R-OWS:ow-scheme) BODY)
       #:with C-TYPE C-TYPE
       (cons #'(C-TYPE NAME (A-OWS ...)) #'R-OWS)]))

  (stx-map mk-ds-item DEF...))

(define get-name
  (syntax-parser
    #:literal-sets [keyword-lits]
    [(class name _ ...) #'name]
    [(field name _ ...) #'name]
    [(def (name _ ...) _) #'name]))
