#lang racket/base

;; ,-,-,-.
;; `,| | |   ,-. . . ,-. ,-. . . ,-. ,-.
;;   | ; | . ,-| | | |   ,-| | | |   | |
;;   '   `-' `-^ `-^ `-' `-^ `-^ `-' `-'
;; Ownership Types Checker.
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
;; Meta:
;; - meta:CS is the set of defined ownership scheme
;; - meta:FS is the map of fields with ownership type field as value
;; - meta:DS is the map of definitions with return ownership type as
;;   value

(require "utils.rkt"
         ;; phases
         "desugar.rkt"
         "meta.rkt"
         "simply-typed.rkt"
         "ownership.rkt")

(provide check)

;; Checks the program for type and ownership and returns its
;; Intermediate Representation with its meta-information.
;;
;; (: check (Syntax -> (Values Syntax meta:CS meta:FS meta:DS)))
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
    ;; Returns prog in IR + meta-info
    (values prog meta:CS meta:FS meta:DS)))
