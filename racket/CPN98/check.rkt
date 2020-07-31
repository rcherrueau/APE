#lang racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.

(require "utils.rkt"
         ;; phases
         "desugar.rkt"
         "meta.rkt"
         "simply-typed.rkt"
         "ownership.rkt")

(provide check)

(define (check prog)
  (let*-values
      (;; Desugaring into the IR
       [(prog)                    (ir> prog)]
       ;; Meta-information
       [(meta:CS meta:FS meta:DS) (M> prog)]
       ;; Simply type Checks
       [(prog)                    (?> prog meta:CS meta:FS meta:DS)]
       ;; Ownership type checks
       [(prog)                    (Θ> prog meta:CS meta:FS meta:DS)])
    (values prog meta:CS meta:FS meta:DS)))
