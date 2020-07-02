#lang racket

(require 2htdp/image
         2htdp/planetcute
         racket/list)

(define (stack imgs)
  (foldr (λ (img tail) (overlay/xy img 0 40 tail))
         (last imgs) (drop-right imgs 1)))

(beside/align
 "bottom"
 (stack (list wall-block-tall stone-block))
 (stack (list character-cat-girl
              stone-block stone-block
              stone-block stone-block))
 water-block
 (stack (list grass-block dirt-block))
 (stack (list grass-block dirt-block dirt-block)))
