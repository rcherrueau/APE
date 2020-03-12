#lang s-exp "../adder+let/lang.rkt"

(let ([x 5]
      [y (add1 x)])
  (sub1 y))