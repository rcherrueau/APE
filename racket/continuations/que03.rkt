;; Inverting back the inversion of control, or Continuation versus
;; page-centric programming.
;; Christian Queinnec
;;
;; @Article{Que03a,
;;   author =       {Christian Queinnec},
;;   title =        {Inverting back the inversion of control or,
;;                   continuations versus page-centric programming},
;;   journal =      {{SIGPLAN} Notices},
;;   year =         2003,
;;   volume =       38,
;;   number =       2,
;;   pages =        {57--64},
;;   url =          {http://doi.acm.org/10.1145/772970.772977},
;;   doi =          {10.1145/772970.772977}
;; }

#lang racket

(require xml
         net/url)

;; ------------------------------------------------------------- utils
(define INVOKE/NO-CONT #f)

(define (make-INVOKE/NO-CONT)
  ((call/cc (λ (k^)
               (set! INVOKE/NO-CONT (λ (th) (k^ th)))
               (λ () 'INVOKE/NO-CONT)))))
(make-INVOKE/NO-CONT)

(define-syntax (λ^ stx)
  (syntax-case stx ()
    [(_ (id ...) e ...)
     #'(lambda (id ...) (INVOKE/NO-CONT (lambda () e ...)))]))

;; http-parse-get: input-port -> string [(key,value)]
(define (http-parse-get in)
  (define req
    (car (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                       (read-line in))))
  (define sub1 (cadr (regexp-split "/" (cadr (regexp-split " " req)))))
  (define sub
    (cond
     ;; Params
     [(regexp-try-match "\\?" (open-input-string sub1))
      (regexp-split "\\?" sub1)]
     ;; No params
     [(> (string-length sub1) 0)
      (list sub1)]
     ;; No params, no dispatch
     [else
      '(#f)]))
  (define dispatch (car sub))
  (define params
    (and
     (eq? 2 (length sub))
     (foldl (λ (param h)
               (let* ([kv (regexp-split "=" param)]
                      [k (car kv)]
                      [v (cadr kv)])
                 (hash-set h k v)))
            (hash)
            (regexp-split "&" (cadr sub)))))
  (regexp-match #rx"(\r\n|^)\r\n" in)
  (values dispatch params))

;; int (input-port output-port -> void) -> shutdown
(define (server port-no handler)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener handler)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener handler)
  (define cust (make-custodian))
  (custodian-limit-memory cust (* 50 1024 1024))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handler in out)
              (close-input-port in)
              (close-output-port out))))
  ;; Watcher thread:
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define ks^ (make-hash))
(define current-connection #f)
(define suicide
  (λ^ ()
      (when (output-port? current-connection)
        (close-output-port current-connection))))

;; --------------------------------------------------------- templates
(define (read-first-num action)
  `(html (head (title "SumServlet I"))
         (body
          (form ([action ,action] [method "get"])
                (table
                 (tr (td "") (td (input ([type "text"] [name "n1"]))))
                 (tr (td "+") (td "")))
                (input ([type "submit"]))))))

(define (read-second-num n1 action)
  `(html
    (head (title "SumServlet II"))
    (body
     (form ([action ,action] [method "get"])
           (table
            (tr (td "") (td ,(number->string n1)))
            (tr (td "+") (td (input ([type "text"] [name "n2"])))))
           (input ([type "hidden"]
                   [name "n1"]
                   [value ,(number->string n1)]))
           (input ([type "submit"]))))))

(define (display-result n1 n2)
  `(html
    (head (title "Result"))
    (body
     (table
      (tr (td "") (td ,(number->string n1)))
      (tr (td "+") (td ,(number->string n2)))
      (tr (td "")
          (td ,(number->string (+  n1 n2))))))))

(define (res-page x y)
  (display "HTTP/1.0 200 Okay\r\n" current-connection)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n"
           current-connection)
  (display (xexpr->string (display-result x y)) current-connection)
  (when (output-port? current-connection)
        (close-output-port current-connection)))

;; -------------------------------------------------------------- apps
;; Handler for "Page Centric Style" programming. The handler parses
;; the Get request and extracts the name of the next page and
;; parameters on `in' port. Handler then, generates the template by
;; calling `dispatcher' getting parameters and prints it on `out'
;; port.
;;
;; The Get request should be of the form:
;;
;; http://sky@www:801/template?name=shriram&host=nw
;;  {-1}   {2} {3} {4}{--5-} {------6-------------}
;; 1 = scheme, 2 = user, 3 = host, 4 = port,
;; 5 = template name,  6 = params
;;
;; handle: input-port output-port -> void
(define (handle in out)
  ;; In "Page Centric Style" programming, the workflow of the web
  ;; application is tangled in "pages" (i.e.: templates). To
  ;; understand the workflow correctly the programmer has to read html
  ;; links in pages.
  ;; dispatcher: template-name params -> xexpr
  (define (dispatcher template-name params)
    (define dispatch-table
      (hash
       "init"
       (λ (params) (read-first-num "gotN1"))
       "gotN1"
       (λ (params) (let ([n1 (string->number (hash-ref params "n1"))])
                     (read-second-num n1 "gotN2")))
       "gotN2"
       (λ (params) (let ([n1 (string->number (hash-ref params "n1"))]
                         [n2 (string->number (hash-ref params "n2"))])
                     (display-result n1 n2)))))
    (cond
     [(false? template-name)
      ((hash-ref dispatch-table "init") params)]
     [else
      ((hash-ref dispatch-table
                 template-name
                 (λ () `(html
                         (head (title "Error"))
                         (body (p "Page not found"))))) params)]))


  ;; Extracts tempalte's name and parameters
  (define-values (template-name params) (http-parse-get in))

  ;; Send reply
  (let ([template (dispatcher template-name params)])
    (display "HTTP/1.0 200 Okay\r\n" out)
    (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
    (display (xexpr->string template) out)))

;; Handler for "Countinuations Style" programming. The handler parses
;; the Get request and extracts the continuation and parameters on
;; `in' port. Handler then, calls the continuation with getting
;; parameter.
;;
;; The Get request should be of the form:
;;
;; http://sky@www:801/template?name=shriram&host=nw
;;  {-1}   {2} {3} {4}{--5-} {------6-------------}
;; 1 = scheme, 2 = user, 3 = host, 4 = port,
;; 5 = continuation id,  6 = params
;;
;; handle^ input-port output-port -> void
(define (handle^ in out)
  (set! current-connection out)
  (define-values (kurl^ params) (http-parse-get in))
  (cond
   [(false? kurl^)
    ((hash-ref ks^ "init") params)]
   [else
    ((hash-ref ks^ kurl^) params)]))

;; Assumption on "Continuation Style" is that programmer writes her
;; web application in an imperative style. Thus, the workflow of the
;; web application is clear. Next is an example of what is expected
;; for our web application.
;;
;; (define n1 0)
;; (set! n1 (read-first-number))
;; (res-page n1 (read-second-number n1))
;;
;; Here is an attempt:
#;
((λ ()
  (call/cc (λ (init^)
             (hash-set! ks^ "init" init^)
             (suicide)))
  (define n1 0)
  (set! n1
        (string->number
         (hash-ref
          (let/cc k^
            (define kurl^ (symbol->string (gensym)))
            (hash-set! ks^ kurl^ k^)
            (display "HTTP/1.0 200 Okay\r\n" current-connection)
            (display "Server: k\r\nContent-Type: text/html\r\n\r\n"
                     current-connection)
            (display (xexpr->string (read-first-num kurl^))
                     current-connection)
            (suicide))
         "n1")))
  (res-page n1
            (string->number
             (hash-ref
              (let/cc k^
                (define kurl^ (symbol->string (gensym)))
                (hash-set! ks^ kurl^ k^)
                (display "HTTP/1.0 200 Okay\r\n" current-connection)
                (display "Server: k\r\nContent-Type: text/html\r\n\r\n"
                         current-connection)
                (display (xexpr->string (read-second-num n1 kurl^) )
                         current-connection)
                (suicide))
              "n2")))))
;;
;; Hopefully, thanks to macros, this code could be made more readable:
(define-syntax (make-web-app stx)
  (syntax-case stx ()
    [(_ e ...)
     #'(let ()
         (let/cc init^
           (hash-set! ks^ "init" init^)
           (suicide))
         e ...)]))

(define-syntax (show stx)
  (syntax-case stx ()
    [(_ f id ...)
     #'(let/cc k^
         (let* ([kurl^ (symbol->string (gensym))]
                [template (f id ... kurl^)])
           (hash-set! ks^ kurl^ k^)
           (display "HTTP/1.0 200 Okay\r\n" current-connection)
           (display "Server: k\r\nContent-Type: text/html\r\n\r\n"
                    current-connection)
           (display (xexpr->string template) current-connection)
           (suicide)))]))

(make-web-app
  (define n1 0)
  (set! n1 (string->number (hash-ref (show read-first-num) "n1")))
  (res-page n1 (string->number (hash-ref (show read-second-num n1)
                                         "n2"))))

;; ------------------------------------------------------------- usage
;; Starts a server on port 8080 with Page Centric Style.
;; (define shut (server 8080 handle))
;; Starts a server on port 8080 with Continuation Style.
;; (define shut (server 8080 handle^))
;; Stop the server.
;; (shut)
