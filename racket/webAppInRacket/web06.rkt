;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web Applications in Racket
;; http://docs.racket-lang.org/continue/
;; 
;; 6. Advanced Control Flow
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
       (> (string-length (extract-binding/single 'title bindings)) 0)
       (exists-binding? 'body bindings)
       (> (string-length (extract-binding/single 'body bindings)) 0)))

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

; render-blog-page: blog request -> doesn't return
; Consumes a blog request and produces an HTML page of the content of the blog
(define (render-blog-page a-blog request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "My Blog"))
                    (body (h1 "My Blog")
                          ,(render-posts a-blog)
                          ; Add post form
                          (form ((action ,(embed/url insert-post-handler)))
                                (input ((name "title")))
                                (input ((name "body")))
                                (input ((type "submit"))))))))

            (define (insert-post-handler request)
            (render-blog-page
             (let ([bindings (request-bindings request)])
               (cond [(can-parse-post? bindings)
                      (cons (parse-post (request-bindings request)) a-blog)]
                     [else
                      a-blog]))
             request))]

  (send/suspend/dispatch response-generator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; start: request -> response
; Consumes a requets and produces a page that displays all the web content
(define (start request)
  (render-blog-page BLOG request))


