#lang typed/racket/base

(require "define-datatype.rkt"
         racket/match
         )

(provide (all-defined-out))

(define-datatype Primitive1
  Add1
  Sub1)

(define-datatype (Exp a)                              ;; `a` is a labeling tag.
  [Num   Number a]                                    ;; Number
  [Id    Symbol a]                                    ;; Identifier
  [Prim1 Primitive1 (Exp a) a]                        ;; Primitive arity 1
  [Let   (Listof (Pairof Symbol (Exp a))) (Exp a) a]  ;; Let* binding
  [If    (Exp a) (Exp a) (Exp a) a]                   ;; if Test Then Else
  )

(: tag-exp (All (a) ((Exp a) -> (Exp Number))))
(define (tag-exp exp)
  (: help (All (a) ((Exp a) Number -> (Values (Exp Number) Number))))
  (define (help exp current-tag)
    (match exp
      [(Num n _)
       (values (Num n current-tag) (add1 current-tag))]
      [(Id s _)
       (values (Id s current-tag) (add1 current-tag))]
      [(Prim1 op exp _)
       (let-values
           ([(exp-tagged next-tag) (help exp (add1 current-tag))])
         (values (Prim1 op exp-tagged current-tag) next-tag))]
      [(Let `((,id . ,val-exp)) exp _)
       (let*-values
           ([(val-exp-tagged next-tag) (help val-exp (add1 current-tag))]
            [(exp-tagged     next-tag) (help exp next-tag)])
         (values (Let `((,id . ,val-exp-tagged)) exp-tagged current-tag) next-tag))]
      [(Let bindings exp tag-exp)
       (let ([b1 (list (car bindings))]
             [bs : (Listof (Pairof Symbol (Exp a))) (cdr bindings)])
         (help (Let b1 (Let bs exp tag-exp) tag-exp) current-tag))]
      [(If test-exp then-exp else-exp _)
       (let*-values
           ([(test-exp-tagged next-tag) (help test-exp (add1 current-tag))]
            [(then-exp-tagged next-tag) (help then-exp next-tag)]
            [(else-exp-tagged next-tag) (help else-exp next-tag)])
         (values (If test-exp-tagged then-exp-tagged else-exp-tagged current-tag) next-tag))]
      [else (error "Tagging Error: Unsupported Exp" exp)]))
  (match/values (help exp 1)
    [(tagged _) tagged]))
