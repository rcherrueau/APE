#lang racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.
;;
;; Naming conventions:
;; - X, Y, FOO (ie, uppercase variables) and `stx' are syntax objects
;; - XS (with an uppercase "S" at the end) is a syntax list of syntax
;;   objects, e.g., #'(a b c 1 v)
;; - Xs (with a small "s" at the end) is a list of syntax objects,
;;   e.g., (list #'a #'b #'c #'1 #'v)
;; - t^ is the ownership scheme of t
;;
;; Phases:
;; - Desugaring phase (ir>) :: Transforms the surface syntax into an
;;   Intermediate Representation.
;; - Meta phase (M>) :: Collects meta information for later use and
;;   checks no duplicate class/field/def names according to [FKF98]
;;   (see Bibliography).
;; - Simple type checking (?>) :: Type checks the program for simple
;;   types ("simple" as in simply typed λ calculus, i.e., no
;;   ownership). Based on [FKF98] (see Bibliography).
;; - Ownership type checking (Θ>) :: Type checks the program for
;;   ownership types. Based on [CPN98] (see Bibliography).
;;
;; Global:
;; - meta:CS is the set of defined ownership scheme
;; - meta:FS is the map of fields with ownership type field as value
;; - meta:DS is the map of definitions with return ownership type as
;;   value

(require "utils.rkt"
         "check.rkt")

(provide (rename-out
          [lang-read read]
          [lang-read-syntax read-syntax]))

;; Reader
(define (lang-read in)
  (syntax->datum (lang-read-syntax #f in)))

(define (lang-read-syntax source-name in)

  ;; Check program, return program in intermediate representation and
  ;; its meta values.
  (define-values (prog-ir meta:CS meta:FS meta:DS)
    (check (port->lines-stx source-name in)))

  ;; Log final AST
  (log-sclang-debug (stx->string prog-ir #:newline? #f))

  ;; Execution
  #`(module cpn98-lang racket/base
      ;; #,prog
        (void)))

;; Profiling lang:
;;
;; Remove compiled directory. Then run with
;; > racket -l errortrace -t lang.rkt
;;
;; (module+ main
;;   (require profile)

;;   (define ctx-params-example
;;      (bytes->path #"/home/rfish/prog/APE/racket/CPN98/examples/ctx-params-example.rkt"))

;;   (profile
;;    (call-with-input-file ctx-params-example
;;      (λ (in)
;;        (read-line in) ;; Strip `#lang ...` from `in`
;;        (lang-read-syntax ctx-params-example in)))
;;    #:repeat 100
;;    #:use-errortrace? #t)
;;   )

;; See, https://github.com/racket/racket/blob/2b567b4488ff92e2bc9c0fbd32bf7e2442cf89dc/pkgs/at-exp-lib/at-exp/lang/reader.rkt#L15
;; (define-values
;;   (surface-read surface-read-syntax surface-get-info)
;;   (make-meta-reader
;;    'surface-lang
;;    "language path"
;;    lang-reader-module-paths
;;    s-reader
;;    TODO...))


;; Tests
(module+ test
  (require (submod "desugar.rkt" test)
           (submod "meta.rkt" test)
           (submod "simply-typed.rkt" test)
           (submod "ownership.rkt" test)))


;; Bibliography
;;
;; @InProceedings{CPN98,
;;   author    = {David G. Clarke and
;;                John Potter and
;;                James Noble},
;;   title     = {Ownership Types for Flexible Alias Protection},
;;   booktitle = {Proceedings of the 1998 {ACM} {SIGPLAN} Conference
;;                on Object-Oriented Programming Systems, Languages
;;                {\&} Applications {(OOPSLA} '98),
;;                Vancouver, British Columbia, Canada, October 18-22, 1998.},
;;   pages     = {48--64},
;;   year      = {1998},
;;   url       = {https://doi.org/10.1145/286936.286947},
;;   doi       = {10.1145/286936.286947}
;; }
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
