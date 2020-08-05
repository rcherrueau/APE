#lang typed/racket/base/no-check

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.
;;
;; Phase (M>)
;;
;; - Collects meta values for later use
;; - Checks no duplicate class/field/def names according to [FKF98]
;;   (see Bibliography).
;; - Check imported modules and collect meta values
;;
;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects
;; - X..., FOO... (ie, uppercase variables with ellipsis) are syntax
;;   objects where `(syntax->datum stx)` would produce a list.

(require racket/contract/base
         racket/function
         racket/lazy-require
         racket/list
         racket/match
         racket/syntax
         syntax/parse
         syntax/parse/define
         syntax/srcloc
         syntax/stx
         "utils.rkt"
         "definitions.rkt")

(lazy-require
 ["check.rkt" (check)])

(provide M>)


;; Phase M>
(define-phase (M> stx)
  (M-prog stx))

(define-rules M-prog
  ;; (import [Prelude "prelude.rkt"]
  ;;         [IO      "io.rkt"]  ...)
  ;; (class ...) ...
  ;; (let ...)
  [(IMPORT CLASS ... E)
   #:cut

   ;; Ensure there is no collision in MODULE names
   #:with (import (NAME:id PATH) ...) #'IMPORT
   #:when (no-name-clash? #'(NAME ...))

   ;; Compute meta values of the class
   (define-values (meta:CS-class meta:FS-class meta:DS-class)
     (M-class #'(CLASS ...)))

   ;; Check modules and get their meta values
   ;; (List (Listof CS) (Listof FS) (Listof DS))
   (match-define
     (list meta:CS-modules
           meta:FS-modules
           meta:DS-modules)
     ;; (: modules (Listof (List CS FS DS)))
     (let* ([modules (stx-map M-module #'([NAME PATH] ...))])
       (if (null? modules) '(() () ())
           (sequence modules))))

   ;; Put everything together
   (values
    (apply append (cons meta:CS-class meta:CS-modules))
    (apply append (cons meta:FS-class meta:FS-modules))
    (apply append (cons meta:DS-class meta:DS-modules)))])

;; Open, check a module and compute meta values.
;;
;; Returns a triplet (List CS FS DS)
(define-rules M-module
  ;; (Prelude "prelude.rkt")
  [(NAME:id PATH:str)
   #:do [(define module-path-relative-to-def-dir (string->path (syntax-e #'PATH)))
         (define-values (definition-dir x y)
           (split-path (srcloc-source (build-source-location #'NAME))))
         (define module-path (simplify-path (build-path definition-dir
                                                        module-path-relative-to-def-dir)))
         ]
   #:fail-unless (file-exists? module-path) "The module should have a valid path"

   ;; Open module and check it
   (define-values (_ meta:CS meta:FS meta:DS)
     (call-with-input-file module-path
       (λ (in)
         (read-line in) ;; Strip `#lang ...` from `in`
         (check (port->lines-stx module-path in)))))

   ;; Prefix TYPE with NAME of the module
   (list
    ;; Meta:CS
    (map (λ (TYPE.CPARAM...)
           (match-define (cons TYPE CPARAM...) TYPE.CPARAM...)

           ;; Append NAME to TYPE
           (define MODULE.TYPE (subst-module-name #'NAME TYPE))

           (cons MODULE.TYPE CPARAM...))
         meta:CS)

    ;; meta:FS
    ;; > (:type FS)
    ;; ((Syntaxof (Pairof TYPE Identifier)) ~> OW-TYPE)
    (meta-map-w/key
     ;; Expand TYPE with module NAME in keys of FS
     ((syntax-parser
        [(TYPE:id . FNAME:id)
         #:with MODULE.TYPE (subst-module-name #'NAME #'TYPE)
         #'(MODULE.TYPE . FNAME)])
      . *** .
      ;; Expend TYPE with module NAME for local type only
      (syntax-parser
        [OT:ow-type #:when (is-local-id? #'OT.TYPE)
         #:with MODULE.TYPE (subst-module-name #'NAME #'OT.TYPE)
         (mk-ow-type #'OT.TYPE #'OT.OWNER #'OT.CPARAMS)]
        [_ this-syntax]))
     meta:FS)

    ;; meta:DS
    ;; > (:type DS)
    ;; ((Syntaxof (List TYPE Identifier (Syntaxof (Listof OW-TYPE)))) ~> OW-TYPE)
    (meta-map-w/key
     ;; Expand TYPE with module NAME in keys of FS
     ((syntax-parser
        [(TYPE:id DNAME:id DARGs)
         #:with MODULE.TYPE (subst-module-name #'NAME #'TYPE)
         #'(MODULE.TYPE DNAME DARGs)])
      . *** .
      ;; Expend TYPE with module NAME for local type only
      (syntax-parser
        [OT:ow-type #:when (is-local-id? #'OT.TYPE)
                    #:with MODULE.TYPE (subst-module-name #'NAME #'OT.TYPE)
                    (mk-ow-type #'OT.TYPE #'OT.OWNER #'OT.CPARAMS)]
        [_ this-syntax]))
     meta:DS))])

;; Check names clash and compute meta values (meta:CS/FS/DS) for the
;; current program.
(define-rules M-class
  [(CLASS ...)
   (for/foldr (;; Accumulators
               ;;
               ;; Set of defined ownership scheme
               ;; (Listof (Pairof Identifier             ; class type
               ;;                 (Listof Identifier)))  ; context parameters
               [meta:CS '()]
               ;; Map of fields with ownership type field as value
               ;; (FS-key ~> OW-TYPE)
               [meta:FS '()]
               ;; Map of definitions with return ownership type as value
               ;; (DS-key ~> OW-TYPE)
               [meta:DS '()]
               ;; Ensure all class types are unique at the end
               #:result
               (when (no-name-clash? meta:CS)
                 (values meta:CS meta:FS meta:DS)))
               ;; Iterate over all CLASS of `stx`.
               ([CLASS (in-syntax #'(CLASS ...))])

      ;; For the current class syntax object, extracts its type (i.e.,
      ;; name), context parameters, and field and def syntax objects.
      (define-values (C-TYPE CPARAM... FIELD... DEF...)
        (get-class-C-TYPE/CPARAM.../FIELD.../DEF... CLASS))

      ;; Ensure all fields and defs are unique in there class
      (no-name-clash? FIELD...)
      (no-name-clash? DEF...)

      ;; Compute the new value for meta:CS, meta:FS, meta:DS
      (values
       ;; meta:CS
       (cons (cons C-TYPE CPARAM...) meta:CS)
       ;; meta:FS
       (append (mk-meta:FS C-TYPE FIELD...) meta:FS)
       ;; meta:DS
       (append (mk-meta:DS C-TYPE DEF...) meta:DS)))])


;; Utils

(: subst-module-name (Identifier Identifier -> Identifier))
(define (subst-module-name MODULE ID)
  (format-id ID "~a.~a" (syntax-e MODULE) (syntax-e ID)
             #:source ID))

;; Get all classes stx
(: get-CLASS... (Syntax -> (Syntaxof (Listof 'class-stx))))
(define get-CLASS...
  (syntax-parser
    #:literal-sets [keyword-lits]
    [((import _ ...) CLASS ... E) #'(CLASS ...)]))

;; Extracts the type, fields and defs syntax objects of a class
(: get-class-C-TYPE/CPARAM.../FIELD.../DEF...
   ('class-stx -> (List Identifier                        ;; class type
                        (Syntaxof (Listof Identifier))    ;; context parameters
                        (Syntaxof (Listof 'field-stx))    ;; Fields stx
                        (Syntaxof (Listof 'def-stx)))))   ;; Defs stx
(define get-class-C-TYPE/CPARAM.../FIELD.../DEF...
  (syntax-parser
    #:literal-sets [keyword-lits]
    [(class ~! NAME [CPARAM ...] FIELD/DEF ...)
     #:with [FIELD ...] (filter field? (stx->list #'(FIELD/DEF ...)))
     #:with [DEF ...] (filter def? (stx->list #'(FIELD/DEF ...)))
     (values #'NAME #'(CPARAM ...) #'(FIELD ...) #'(DEF ...))]))

;; Transforms a list of field stx objects `FIELD...` of a specific
;; class `C-TYPE` into an associative list of `FS-key` and
;; `OW-TYPE`.
(: mk-meta:FS (Identifier (Syntaxof (Listof 'field-stx))
                          -> (Listof (Pairof FS-key OW-TYPE))))
(define (mk-meta:FS C-TYPE FIELD...)
  (define mk-fs-item (syntax-parser
      #:literal-sets [keyword-lits]
      [(field NAME OWS:ow-type)
       #:with C-TYPE C-TYPE
       (cons #'(C-TYPE . NAME) #'OWS)]))

  (stx-map mk-fs-item FIELD...))

;; Transforms a list of def stx objects `DEF...` of a specific class
;; `C-TYPE` into an associative list of `DS-key` and `OW-TYPE`.
(: mk-meta:DS (Identifier (Syntaxof (Listof 'def-stx))
                          -> (Listof (Pairof DS-key OW-TYPE))))
(define (mk-meta:DS C-TYPE DEF...)
  (define mk-ds-item (syntax-parser
      #:literal-sets [keyword-lits]
      [(def (NAME (A-NAME A-OWS:ow-type) ...  R-OWS:ow-type) _ ...)
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
      [(def (name:id _ ...) _ ...) #'name]
      [(name:id . (ctx-params:id ...)) #'name]
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

  (run-tests
   (test-suite
    "Meta phase (M>)"

    ;; Check no-name-clash
    (check-true (no-name-clash? #'(foo bar)))                            ;; Modules
    (check-true (no-name-clash? #'((foo . ()) (bar . (c1 c2)))))         ;; CS
    (check-true (no-name-clash? #'((field foo) (field bar))))
    (check-true (no-name-clash? #'((def (foo) ???) (def (bar) ???))))

    (check-exn exn:name-clash? (thunk (no-name-clash? #'(foo foo))))
    (check-exn exn:name-clash? (thunk (no-name-clash? #'((field foo) (field foo)))))
    (check-exn exn:name-clash? (thunk (no-name-clash? #'((def (foo) ???) (def (foo) ???)))))

    (check-exn exn:fail:syntax? (thunk (no-name-clash? #'(((foo)))))
               "`(foo)` is not a valid name")
    (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((filed (foo)))))
               "`filed` is not a `field`")
    (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((field (foo)))))
               "`(foo)` is not a valid name for field")
    (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((dEf (foo) ???))))
               "`dEf` is not a `def`")
    (check-exn exn:fail:syntax? (thunk (no-name-clash? #'((def ((foo)) ???))))
               "`(foo)` is not a valid name for def")

    ;; Check (M>)
    (check-exn exn:name-clash?
               (thunk (M> #'((import)
                             (class Foo [])
                             (class Foo [])
                             expr)))
               "Classes should have a unique name")
    (check-exn exn:name-clash?
               (thunk (M> #'((import)
                             (class Foo []
                               (field foo (t o {}))
                               (field foo (t o {})))
                             expr)))
               "Fields of a class should have a unique name")
    (check-exn exn:name-clash?
               (thunk (M> #'((import)
                             (class Foo []
                               (def (foo (Foo o ())) ???)
                               (def (foo (Foo o ())) ???))
                             expr)))
               "Methods of a class should have a unique name")

    (check-not-exn (thunk (M> #'((import)
                                 (class Foo [] (field foo (t o {})))
                                 (class Bar [] (field bar (t o {})))
                                 expr))))

    (check-not-exn (thunk (M> #'((import)
                                 (class Foo [] (field foo (t o {})))
                                 (class Bar [] (field foo (t o {})))
                                 expr)))
                   "Two fields could have the same name in different classes")

    (check-not-exn (thunk (M> #'((import)
                                 (class Foo []
                                   (field foo (t o {}))
                                   (def (foo (Foo o ())) ???))
                                 expr)))
                   "A field and a def could have the same name"))))


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
