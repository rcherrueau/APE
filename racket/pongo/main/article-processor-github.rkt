#lang racket

(require net/url)

; URL to github markdown parser.
(define markdown-service
  (string->url "https://api.github.com/markdown/raw"))

; processes: article -> generated-content
; Take an markdown article as input-port and returns the html result
; on the input-port.
; Example usage:
;  (copy-port (processes (open-input-file "../tests/sample_content.mrk"))
;             (open-output-file "tmptmp.html"))
;
;  (port->list read-line
;              (processes (open-input-file "../tests/sample_content.mrk")))
(define (processes article)
  (post-pure-port markdown-service
                  (port->bytes article)
                  (list "Content-Type: text/plain")))

(provide (all-defined-out))
