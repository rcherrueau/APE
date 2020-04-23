#lang typed/racket/base/no-check

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.
;;
;; Collect meta information for later use (M>).
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

(provide meta:CS meta:FS meta:DS
         M>)


;; Meta values

(: meta:CS (Listof Identifier))
(define meta:CS #f)

(: meta:FS (Dict FS-key OW-SCHEME))
(define meta:FS #f)

(: meta:DS (Dict DS-key OW-SCHEME))
(define meta:DS #f)


;; Function to find and set meta values

(define (M> stx)
  (set!-values

   (meta:CS meta:FS meta:DS)

   (for/foldr ([cs '()][fs '()][ds '()])
              ([class-stx (in-syntax (get-classes-stx stx))])
     (define-values (ctype fields-stx defs-stx)
       (get-ctype/fields/defs-stx class-stx))
     (values
      ;; cs
      (cons ctype cs)
      ;; fs
      (append (mk-fs ctype fields-stx) fs)
      ;; ds
      (append (mk-ds ctype defs-stx) ds)))))


;; Utils

;; Get all classes stx
(: get-classes-stx (Syntax -> (Syntaxof (Listof 'class-stx))))
(define get-classes-stx
  (syntax-parser
    #:literal-sets [keyword-lits]
    [(prog ~! CLASS ... E) #'(CLASS ...)]))

;; Extracts the type, fields and defs syntax objects of a class
(: get-ctype/fields/defs-stx
   ('class-stx -> (List Identifier                        ;; class type
                        (Syntaxof (Listof 'field-stx))    ;; Fields stx
                        (Syntaxof (Listof 'def-stx)))))   ;; Defs stx
(define get-ctype/fields/defs-stx
  (syntax-parser
    #:literal-sets [keyword-lits]
    [(class ~! NAME [CPARAM ...] FIELD/DEF ...)
     #:with [FIELD ...] (filter field? (stx->list #'(FIELD/DEF ...)))
     #:with [DEF ...] (filter def? (stx->list #'(FIELD/DEF ...)))
     (values #'NAME #'(FIELD ...) #'(DEF ...))]))

(: mk-fs
   ((Pairof Identifier (Syntaxof (Listof 'field-stx)))
    -> (Listof (Pairof FS-key OW-SCHEME))))
(define (mk-fs C-TYPE FIELD...)
  (define mk-fs-item (syntax-parser
      #:literal-sets [keyword-lits]
      [(field NAME OWS:ow-scheme)
       #:with C-TYPE C-TYPE
       (cons #'(C-TYPE . NAME) #'OWS)]))

  (stx-map mk-fs-item FIELD...))

(: mk-ds
   ((Pairof Identifier (Syntaxof (Listof 'def-stx)))
    -> (Listof (Pairof DS-key OW-SCHEME))))
(define (mk-ds C-TYPE DEF...)
  (define mk-ds-item (syntax-parser
      #:literal-sets [keyword-lits]
      [(def (NAME (A-NAME A-OWS:ow-scheme) ...  R-OWS:ow-scheme) BODY)
       #:with C-TYPE C-TYPE
       (cons #'(C-TYPE NAME (A-OWS ...)) #'R-OWS)]))

  (stx-map mk-ds-item DEF...))
