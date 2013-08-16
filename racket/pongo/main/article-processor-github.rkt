#lang racket

(require net/url)

; URL to github markdown parser.
(define markdown-service
  (string->url "https://api.github.com/markdown/raw"))

; processes: article -> generated-content
; Take an markdown article and returns the html result on the
; input-port.
; Example usage:
;  (copy-port (processes (file->string "../tests/sample_content.mrk"))
;             (open-output-file "tmptmp.html"))
;
;  (port->list read-line
;              (processes (file->string "../tests/sample_content.mrk")))
(define (processes article)
  (post-pure-port markdown-service
                  (string->bytes/locale article)
                  (list "Content-Type: text/plain")))
