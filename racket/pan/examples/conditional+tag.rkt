#lang s-exp "../conditional+tag/lang.rkt"

(let ([x (if 10 1 0)]
      [y (sub1 x)])
  (if y 55 999))
