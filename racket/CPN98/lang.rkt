#lang racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.

(require "utils.rkt"
         "desugar.rkt"
         "meta.rkt"
         "simply-typed.rkt"
         "ownership.rkt"
         )

(provide (rename-out
          [lang-read read]
          [lang-read-syntax read-syntax]))

;; Reader
(define (lang-read in)
  (syntax->datum (lang-read-syntax #f in)))

(define (lang-read-syntax src in)
  (define (s-exps rs)
     (let s-exps ([s-exp (rs src in)])
       (if (eof-object? s-exp)
           '()
           (cons s-exp (s-exps (rs src in))))))

  ;; ~~~~~~~~~~~~~~~~
  ;; Tower of transformation
  (let*
      (;; Prog in surface syntax
       [prog #`(prog #,@(s-exps read-syntax))]
       ;; Desugaring
       [prog (∗> prog)]
       ;; Meta-information (Effectful computation)
       [_    (M> prog)]
       ;; Basic Checks
       [prog (?> prog)]
       ;; Ownership
       [prog (θ> prog)]
       )

    ;; Log final AST
    (log-sclang-debug (stx->string prog #:newline? #f))

    ;; Execution
    #`(module cpn98-lang racket/base
        ;; #,prog
        (void)
        )))

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
