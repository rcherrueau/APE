#lang racket/base

(provide (all-defined-out))

(require redex)

;; Reduction sequence printer.
;;
;; /Note/ this function print one reduction sequence. To see all
;; availbale possibilities, see `traces`.
;; reduction-sequence: red-rel red-rel-name -> term -> void
(define (reduction-sequence r r-name)
  (define (_red-seq term [nest 0])
    (define rts (apply-reduction-relation r term))
    (cond
     [(null? rts)
      (cond
       [(equal? nest 0)
        (printf "No reduction for ~a~n" term)]
       [else
        (void)])]
     [else
      (cond
       [(equal? nest 0)
        (define red-term (car rts))
        (define new-nest (string-length (format "~a" term)))
        (printf "~a ~a ~a~n" term r-name red-term)
        (_red-seq red-term new-nest)]
       [else
        (define red-term (car rts))
        (printf "~a ~a ~a~n"
                (make-string nest #\space)
                r-name
                red-term)
        (_red-seq red-term nest)])]))
  _red-seq)
