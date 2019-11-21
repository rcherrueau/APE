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
         "basic-checks.rkt"
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
  ;; Tower of transfo

  ;; Vanilla prog
  (define prog  #`(prog #,@(s-exps read-syntax)))
  ;; Desugaring
  (define *prog (∗> prog))
  ;; Meta-information
  (define-values (CS FS DS) (M> *prog))
  (parameterize ([private:CS CS][private:FS FS][private:DS DS])
    ;; Basic Checks
    (define ?prog (?> *prog))
    ;; Datum
    (define datum-res (syntax->datum ?prog))

    ;; Pretty print
    (define datum-str
      (call-with-output-string
       (λ (out-str) (pretty-print datum-res out-str))))

    ;; Execution
    #`(module cpn98-lang racket/base
        (time (display #,datum-str))))
  )


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
