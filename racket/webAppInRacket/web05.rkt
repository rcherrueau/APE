;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web Applications in Racket
;; http://docs.racket-lang.org/continue/
;; 
;; 5. Inspecting Requests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang web-server/insta 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blog Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Blog post data-structure
(struct post(title body))

; BLOG: blog
; The static blog
(define BLOG 
  (list (post "Second Post" "This is another post")
        (post "First Post" "This is my first post")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blog Bindings & Params
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; can-parse-post?: bindings -> boolean
; Test if bindings for a blog post are provided
(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
       (exists-binding? 'body bindings)))

; parse-post: bindings -> post
; Consumes a bindings and produce a blog post out of the bindings
(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blog Render Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; render-post: post -> xexpr
; Consumes a blog post and produce an xexpr fragment
(define (render-post post)
  `(div ((class "post")) ,(post-title post) (p ,(post-body post))))

; render-posts: (listof post)  -> xexpr
; Consume a list of post and produce an xexpr fragment
(define (render-posts posts)
  ; render-as-itemized-list: (listof xexpr) -> xexpr
  ; Consumes a list of items, and produces a rendering
  ; as an unordered list.
  (define (render-as-itemized-list fragments)
    `(ul ,@(map render-as-item fragments)))
 
  ; render-as-item: xexpr -> xexpr
  ; Consumes an xexpr, and produces a rendering
  ; as a list item.
  (define (render-as-item a-fragment)
    `(li ,a-fragment))
  
  `(div ((class "posts"))
        ,(render-as-itemized-list (map render-post posts))))

; render-add-post-form: xexpr -> xexpr
(define (render-add-post-form a-fragment)
  `(div ,a-fragment (form ((class "add-post-form"))
                          (input ((name "title")))
                          (input ((name "body")))
                          (input ((type "submit"))))))

; render-blog-page: blog request -> response
; Consumes a blog request and produces an HTML page of the content of the blog
(define (render-blog-page a-blog request)
  (response/xexpr
   `(html (head (title "My Blog"))
          (body (h1 "My Blog")  
                ,(render-add-post-form (render-posts a-blog))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; start: request -> response
; Consumes a requets and produces a page that displays all the web content
(define (start request)
  (let* ([bindings (request-bindings request)]
         [a-blog (cond 
                  [(can-parse-post? bindings)
                   (cons (parse-post bindings) BLOG)]
                  [else BLOG])])
    (render-blog-page a-blog request)))


