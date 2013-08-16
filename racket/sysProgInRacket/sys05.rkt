;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Systems Programming with Racket
;; http://docs.racket-lang.org/more/
;; 
;; 5. Server Thread
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

; serve: port-no -> stop
; Start a new TCP server listening on port port-no. Serve 
; returns a function that can be used to shut down the server.
(define (serve port-no)
  
  ; Create a "listening" server on the local machin with
  ; maximum 5 simultaneous connection.
  (define listener (tcp-listen port-no 5 #t))
  
  ; Loop to accept connections from listeners.
  (define (loop)
    (accept-and-handle listener)
    (loop))
  
  ; Listener loop in its own thread, so
  ; (serve port-no) in repl will return immediately.
  (define t (thread loop))

  ; Function for shut down the server thread and
  ; tcp listener
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

; accept-and-handle: listener -> in out
; Accept a connection and process input (request)
; and output (respone).
(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (handle in out)
  (close-input-port in)
  (close-output-port out))

; handle: in out -> doesn't return
; Read and discard the request header and then write 
; "Hello World!" web page as a result.
(define (handle in out)
  ; Discard the request header (up to blank line
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ; Send reply
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))