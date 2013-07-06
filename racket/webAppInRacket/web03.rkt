;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web Applications in Racket
;; http://docs.racket-lang.org/continue/
;; 
;; 3. Basic Blog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang web-server/insta

; Blog post data-structure
(struct post(title body))

; BLOG: blog
; The static blog
(define BLOG 
  (list (post "Second Post" "This is another post")
        (post "First Post" "This is my first post")))

; start: request -> dosen't return
; Consume a request and return an "Under Construction" HTML page.
(define (start request)
  (response/xexpr
   '(html
     (head (title "My Blog"))
     (body (h1 "Under construction")))))