#lang racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.

(require racket/port
         racket/pretty
         "utils.rkt"
         "definitions.rkt"
         "desugar.rkt"
         "meta.rkt"
         "basic-check.rkt"
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
       ;; Meta-information
       [_    (set-box!-values (meta:CS meta:FS meta:DS) (M> prog))]
       ;; Basic Checks
       [prog (?> prog)]
       ;; ;; Ownership
       ;; #;[prog (θ> prog)]
       ;; Final AST
       [prog (stx->string prog)])
    ;; Execution
    #`(module cpn98-lang racket/base
        (time (display #,prog)))))

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
