#lang racket

(require net/url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artcile Processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; URL to github markdown parser.
(define markdown-service
  (string->url "https://api.github.com/markdown/raw"))

; processes: article -> generated-content
; Take an markdown article as input-port and returns the html result
; on the input-port.
; Example usage:
;  (copy-port (processes (open-input-file
;                          "../tests/2013-08-20-MySuperTitle.mrk"))
;             (open-output-file
;               "../tests/2013-08-20-MySuperTitle.html"
;               #:exists 'replace))
;
;  (port->list read-line (processes (open-input-file
;    "../tests/2013-08-20-MySuperTitle.mrk")))
(define (processes article)
  (post-pure-port markdown-service
                  (port->bytes article)
                  (list "Content-Type: text/plain")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide processes)
