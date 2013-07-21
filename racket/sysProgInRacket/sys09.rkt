;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Systems Programming with Racket
;; http://docs.racket-lang.org/more/
;; 
;; 9. Servlets and Sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require xml net/url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
             (define (build-request-page label next-url)
               `(html
                 (head (title "Enter a Number to Add"))
                 (body
                       (form ([action ,next-url] [method "get"])
                             ,label
                             (input ([type "text"]
                                     [name "number"]
                                     [value ""]))
                             (input ([type "submit"]))))))
             (build-request-page "Number of greetings:" "/reply")))

; Dispatcher for reply
(hash-set! dispatch-table "reply"
           (lambda (query)
             (define n (string->number (cdr (assq 'number query))))
             `(html (body ,@(for/list ([i n]) "hello ")))))

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
    (let ([xexpr (dispatch (list-ref req 1))])
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
