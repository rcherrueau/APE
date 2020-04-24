#lang typed/racket/base/no-check

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.
;;
;; Phase (M>)
;;
;; Collects meta information for later use and checks no duplicate
;; class/field/def names according to [FKF98] (see Bibliography).
;;
;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects
;; - X..., FOO... (ie, uppercase variables with ellipsis) are syntax
;;   objects where `(syntax->datum stx)` would produce a list.

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


;; Phase M>

;; Check names clash and set meta values (using global meta:CS/FS/DS).
;;
;; (: M> (Syntax -> Void))
(define (M> stx)
  (set!-values

   (meta:CS meta:FS meta:DS)

   (for/foldr ([cs '()][fs '()][ds '()])
              ([CLASS (in-syntax (get-CLASS... stx))])
     ;; For the current class syntax object, extracts its type (i.e.,
     ;; name) field and def syntax objects.
     (define-values (C-TYPE FIELD... DEF...)
       (get-class-C-TYPE/FIELD.../DEF... CLASS))

     ;; Ensure all fields and defs are unique in there class
     (no-name-clash? FIELD...)
     (no-name-clash? DEF...)

     ;; Compute the new value for cs, fs, ds
     (values
      ;; cs
      (cons C-TYPE cs)
      ;; fs
      (append (mk-fs C-TYPE FIELD...) fs)
      ;; ds
      (append (mk-ds C-TYPE DEF...) ds))))

  ;; Ensure all class types are unique
  (no-name-clash? meta:CS)
  )


;; Utils

;; Get all classes stx
(: get-CLASS... (Syntax -> (Syntaxof (Listof 'class-stx))))
(define get-CLASS...
  (syntax-parser
    #:literal-sets [keyword-lits]
    [(prog ~! CLASS ... E) #'(CLASS ...)]))

;; Extracts the type, fields and defs syntax objects of a class
(: get-class-C-TYPE/FIELD.../DEF...
   ('class-stx -> (List Identifier                        ;; class type
                        (Syntaxof (Listof 'field-stx))    ;; Fields stx
                        (Syntaxof (Listof 'def-stx)))))   ;; Defs stx
(define get-class-C-TYPE/FIELD.../DEF...
  (syntax-parser
    #:literal-sets [keyword-lits]
    [(class ~! NAME [CPARAM ...] FIELD/DEF ...)
     #:with [FIELD ...] (filter field? (stx->list #'(FIELD/DEF ...)))
     #:with [DEF ...] (filter def? (stx->list #'(FIELD/DEF ...)))
     (values #'NAME #'(FIELD ...) #'(DEF ...))]))

;; Transforms a list of field stx objects `FIELD...` of a specific
;; class `C-TYPE` into an associative list of `FS-key` and
;; `OW-SCHEME`.
(: mk-fs (Identifier (Syntaxof (Listof 'field-stx))
                     -> (Listof (Pairof FS-key OW-SCHEME))))
(define (mk-fs C-TYPE FIELD...)
  (define mk-fs-item (syntax-parser
      #:literal-sets [keyword-lits]
      [(field NAME OWS:ow-scheme)
       #:with C-TYPE C-TYPE
       (cons #'(C-TYPE . NAME) #'OWS)]))

  (stx-map mk-fs-item FIELD...))

;; Transforms a list of def stx objects `DEF...` of a specific class
;; `C-TYPE` into an associative list of `DS-key` and `OW-SCHEME`.
(: mk-ds (Identifier (Syntaxof (Listof 'def-stx))
                     -> (Listof (Pairof DS-key OW-SCHEME))))
(define (mk-ds C-TYPE DEF...)
  (define mk-ds-item (syntax-parser
      #:literal-sets [keyword-lits]
      [(def (NAME (A-NAME A-OWS:ow-scheme) ...  R-OWS:ow-scheme) BODY)
       #:with C-TYPE C-TYPE
       (cons #'(C-TYPE NAME (A-OWS ...)) #'R-OWS)]))

  (stx-map mk-ds-item DEF...))


;; Exceptions

;; Name clash
(struct exn:name-clash exn:fail:syntax ()
  #:extra-constructor-name make-exn:name-clash
  #:transparent)

(define (raise-name-clash ID IDs)
  (define srcloc-msg (srcloc->string (build-source-location ID)))
  (define id (format "~s" (syntax->datum ID)))
  (define err-msg "multiple declaration")
  (define previous-ID (findf (curry bound-id=? ID) IDs))
  (define previous-ID-msg (format "~n  previously seen at line ~a:~a"
                                  (syntax-line previous-ID)
                                  (syntax-column previous-ID)))

  (raise (make-exn:name-clash
          (string-append srcloc-msg ": " id ": " err-msg previous-ID-msg)
          (current-continuation-marks)
          (list (syntax-taint ID)))))

;; Tests if there is no duplicated name declarations, or raises an
;; exception otherwise.
(: no-name-clash?
   ((U (Listof Identifier)            ;; Class types
      (Syntaxof (Listof 'field-stx))  ;; Fields stx of a specific class
      (Syntaxof (Listof 'def-stx)))   ;; Defs stx of a specific class
    -> Boolean))
(define (no-name-clash? stxs)
  ;; Extract names from a `class`, `field` or `def`
  (define get-name
    (syntax-parser
      #:literal-sets [keyword-lits]
      [(field name:id _ ...) #'name]
      [(def (name:id _ ...) _) #'name]
      [name:id #'name]))

  ;; Extract names from `stxs`
  (define names (stx-map get-name stxs))

  (cond
    ;; A duplicate name exists
    [(check-duplicate-identifier names)
     => (λ (name) (raise-name-clash name names))]
    ;; Everything is fine
    [else #t]))


(module+ test
  (require rackunit rackunit/text-ui)
  (provide meta-tests)

  (define meta-tests
    (test-suite
     "Meta phase (M>)"

     ;; TODO:
     ;; ;; Check no-name-clash
     ;; (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((clazz foo))))
     ;;            "`clazz` is not a `class`")
     ;; (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((class (foo)))))
     ;;            "`(foo)` is not a valid name")
     ;; (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((filed foo))))
     ;;            "`filed` is not a `field`")
     ;; (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((field (foo)))))
     ;;            "`(foo)` is not a valid name for field")
     ;; (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((dEf (foo) ???))))
     ;;            "`dEf` is not a `def`")
     ;; (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((def ((foo)) ???))))
     ;;            "`(foo)` is not a valid name for def")

     ;; (check-true (no-name-clash? #'((class foo) (class bar))))
     ;; (check-true (no-name-clash? #'((field foo) (field bar))))
     ;; (check-true (no-name-clash? #'((def (foo) ???) (def (bar) ???))))

     ;; (check-exn exn:name-clash? (thunk (no-name-clash? #'((class foo) (class foo)))))
     ;; (check-exn exn:name-clash? (thunk (no-name-clash? #'((field foo) (field foo)))))
     ;; (check-exn exn:name-clash? (thunk (no-name-clash? #'((def (foo) ???) (def (foo) ???)))))


     ;; ;; Check (M>)
     ;; (check-true (no-name-clash? #'((class foo (field foo)) (class bar (field bar)))))

     ;; (check-true (no-name-clash? #'((class foo (field foo)) (class bar (field foo))))
     ;;             "Two field could have the same name in different class")
     ;; (check-true (no-name-clash? #'((class foo (field foo) (def foo (foo))))))
     ;; ;; (check-true (no-name-clash? #'((class foo (field foo) (field foo)))))
     ))
  (run-tests meta-tests)
  )


;; Bibliography
;;
;; @InProceedings{FKF98,
;;   author =       {Matthew Flatt and Shriram Krishnamurthi and Matthias
;;                   Felleisen},
;;   title =        {Classes and Mixins},
;;   booktitle =    {{POPL} '98, Proceedings of the 25th {ACM}
;;                   {SIGPLAN-SIGACT} Symposium on Principles of
;;                   Programming Languages, San Diego, CA, USA, January
;;                   19-21, 1998},
;;   year =         1998,
;;   pages =        {171--183},
;;   doi =          {10.1145/268946.268961},
;;   url =          {https://doi.org/10.1145/268946.268961},
;; }
