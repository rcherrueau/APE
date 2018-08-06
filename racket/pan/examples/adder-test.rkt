#lang s-exp "../adder/lang.rkt"

;; Adder -- A starter language.
;;
;; See:
;; - https://course.ccs.neu.edu/cs4410/lec_let-and-stack_notes.html
;; - https://course.ccs.neu.edu/cs4410/hw_02_assignment.html

;; n  ∈ Nat
;;
;; exp ::= n              (Num)
;;       | (add1 e)       (Prim1 Add1)
;;       | (sub1 e)       (Prim1 Sub1)

(sub1 (add1 (add1 42)))
