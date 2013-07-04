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
  ;(phase-1 request))
  (send/suspend
   (lambda (k-url)
     (case
       [(equal? k-url (request-uri request)) 
        (response/xexpr
         `(html (head (title "Action"))
                (body (h1 "Action"))))]
       [else 
        (response/xexpr
         `(html (head (title "Enter a number"))
                (body
                 (form ([action ,k-url])
                       "Enter a number: "
                       (input ([name "number"]))
                       (input ([type "submit"]))))))]))))

