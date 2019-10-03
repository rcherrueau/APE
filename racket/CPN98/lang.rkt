#lang racket/base

(require racket/port
         racket/pretty
         "desugar.rkt"
         "basic-checks.rkt")

(provide (rename-out
          [lang-read read]
          [lang-read-syntax read-syntax]))

;; Reader
(define (lang-read in)
  (syntax->datum (lang-read-syntax #f in)))

(define (lang-read-syntax src in)

  ;; Vanilla prog
  (define s-exps
    (let s-exps ([s-exp (read-syntax src in)])
      (if (eof-object? s-exp)
          '()
          (cons s-exp (s-exps (read-syntax src in))))))

  (let* (;; Tower of transfo
         [prog  (quasisyntax/loc (car s-exps) (prog #,@s-exps))]  ;; Vanilla prog
         [*prog (∗> prog)]                                        ;; Desugaring
         [?prog (?> *prog)]                                       ;; Basic Checks
         [datum-res (syntax->datum ?prog)]                        ;; Datum

         ;; Pretty print
         [datum-str (call-with-output-string
                   (λ (out-str) (pretty-print datum-res out-str)))])

    #`(module cpn88-lang racket/base
        (require racket/pretty)
        (display #,datum-str))))
