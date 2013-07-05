#lang web-server/insta

; phase-2: request -> response
(define (phase-2 request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html
               (body (h1 "Phase 2")
                     (a ((href ,(embed/url phase-1)))
                        "click me!")))))]
    (send/suspend/dispatch response-generator)))

; phase-1: request -> response
(define (phase-1 request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html
               (body (h1 "Phase 1")
                     (a ((href ,(embed/url phase-2)))
                        "click me!")))))]
    (send/suspend/dispatch response-generator)))

; start: request -> response
(define (start request)
  (phase-1 request))

