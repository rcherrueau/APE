;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Systems Programming with Racket
;; http://docs.racket-lang.org/more/
;; 
;; 11. Continuations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require xml net/url)
(require racket/control)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; build-request-page: label next-url hidden -> xexpr
; Consruct an HTML form. The label argument us a string to show the user.
; The next-url argument is a destination for the form results. The hidden
; argument is a value to propagate throught the form as a hidden field.
(define (build-request-page label next-url hidden)
  `(html
    (head (title "Enter a Number to Add"))
    (body
     (form ([action ,next-url] [method "get"])
           ,label
           (input ([type "text"][name "number"][value ""]))
           (input ([type "hidden"][name "hidden"][value ,hidden]))
           (input ([type "submit"]))))))

; Hash Table for web dispatching
; Value is a lambda (list of (symbol value)) -> xexpr
; that takes a list of key val computed from the url
; and returns an xexpr expresion
(define dispatch-table (make-hash))

; Dispatcher for hello
(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "Hello, World!"))))



; Dispatcher for many
(hash-set! dispatch-table "many"
           (lambda (query)
             (build-request-page "Number of greetings:" "/reply")))

; Dispatcher for reply
(hash-set! dispatch-table "reply"
           (lambda (query)
             (define n (string->number (cdr (assq 'number query))))
             `(html (body ,@(for/list ([i n]) "hello ")))))


; Servlet that takes two numbers to add by using the hidden field
; in the form to remember the first number: 
(define (sum query)
  (build-request-page "First number:" "/one" ""))

(define (one query)
  (build-request-page "Second number:" "/two" (cdr (assq 'number query))))

(define (two query)
  (define n (string->number (cdr (assq 'number query))))
  (define m (string->number (cdr (assq 'hidden query))))
  `(html
    (head (title "Sum of one and two"))
    (body "The sum is: " ,(number->string (+ m n)))))

(hash-set! dispatch-table "sum" sum)
(hash-set! dispatch-table "one" one)
(hash-set! dispatch-table "two" two)

; Same as above but in a direct style:

; 
(define (get-number label)
  (define query
    ; Generate a URL for the current computation:
    (send/suspend
     ; Receive the computation-as-URL here:
     (lambda (k-url)
       ; Generate the query-page result for this connection.
       ; Send the query result to the saved-computation URL:
       (build-request-page label k-url ""))))
  ; We arrive here later, in a new connection
  (string->number (cdr (assq'number query))))

(define (sum2 query)
  (define m (get-number "First number:"))
  (define n (get-number "Second number:"))
  `(html
    (head (title "Sum of one and two"))
    (body "The sum is: " ,(number->string (+ m n)))))

(hash-set! dispatch-table "sum2" sum2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; serve: port-no -> stop
; Start a new TCP server listening on port port-no. Serve 
; returns a function that can be used to shut down the server.
(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    ; Create a "listening" server on the local machin with
    ; maximum 5 simultaneous connection.
    (define listener (tcp-listen port-no 5 #t))
  
    ; Loop to accept connections from listeners.
    (define (loop)
      (accept-and-handle listener)
      (loop))
  
    ; Listener loop in its own thread, so
    ; (serve port-no) in repl will return immediately.
    (thread loop))

  ; Function for shut down the server and close 
  ; resources in the custodian.
  (lambda ()
    (custodian-shutdown-all main-cust)))

; accept-and-handle: listener -> in out
; Accept a connection and process input (request)
; and output (respone).
(define (accept-and-handle listener)
  ; Container for thread and in out streams 
  (define cust (make-custodian))
  ; Limit the memory use of a connection to 50mb
  (custodian-limit-memory cust (* 50 1024 1024))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread
     (lambda ()
       ;; (sleep (random 10))
       (handle in out)
       (close-input-port in)
       (close-output-port out))))
  
  ; Wait for 10 second then close all elements in the custodian.
  ; This is a poor implementation of TTL.
  (thread
   (lambda()
     (sleep 10)
     (custodian-shutdown-all cust))))

; handle: in out -> doesn't return
; Read and discard the request header and then use dispatcher
; to write page in turn.
(define (handle in out)
  (define req
    ; Match the first line to extract the request:
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  
  (when req
    ; Discard the rest of the header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ; Dispatch
    ; prompt mark the place where a servlet is started. So that
    ; we can abrt a computation to that point (see continuation:
    ; http://docs.racket-lang.org/reference/eval-model.html#(part._cont-model)
    ; http://docs.racket-lang.org/reference/eval-model.html#%28part._prompt-model%29
    (let ([xexpr (prompt (dispatch (list-ref req 1)))])
      ; Send reply
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

; dispatch: str-path -> xexpr
; Take a string, interpret it as an url and look at the first element
; to generate a web page xexpr.
(define (dispatch str-path)
  ; Parse the request as a URL
  (define url (string->url str-path))
  ; Extract the path part
  (define path (map path/param-path (url-path url)))
  ; Find a handler based on the path's first element
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ; Call a handler
      (h (url-query url))
      ; No handler found
      `(html (head (title "Error"))
             (body 
              (font ((color "red"))
                    "Unknown page: " ,str-path)))))