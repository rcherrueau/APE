;; From https://klibert.pl/posts/adding_string_interpolation_to_racket.html
;;
;; See also
;; https://docs.racket-lang.org/guide/syntax_module-reader.html?q=s-exp%20syntax%2Fmodule-reader
;; and
;; https://github.com/racket/racket/blob/2b567b4488ff92e2bc9c0fbd32bf7e2442cf89dc/pkgs/at-exp-lib/at-exp/lang/reader.rkt#L15

#lang s-exp syntax/module-reader
#:language read
#:wrapper1 wrap-read

;; Read PATTERN in `#@(PATTERN)' and wrap it with `stx/surface' with
;; with `current-syntax-context' as origin.
(define read-#@pattern
  (case-lambda
    [(char in src line col pos)
     (let* ([pattern (read in)]
            [stx `(stx/surface ,pattern (current-syntax-context))])
       (datum->syntax #f stx))]
    [(char in) (read-#@pattern char in "" 0 0 0)]))

;; Make a syntax object with the loc of `this-syntax`.
(define (wrap-read do-read)
  (define rt (make-readtable (current-readtable)
                             #\@ 'dispatch-macro
                             read-#@pattern))

  (parameterize ([current-readtable rt])
    (do-read)))
