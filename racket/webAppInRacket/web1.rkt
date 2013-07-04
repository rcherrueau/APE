#lang web-server/insta

;; render-greeting: string -> response
;; Consumes a name and produces a dynamic response.
;(define (render-greeting a-name)
;  (response/xexpr
;   `(html (head (title "My Blog"))
;          (body (p ,(string-append "Hello " a-name))))))

; Blog post data-structure
(struct post(title body))

; BLOG: blog
; The static blog
(define BLOG 
  (list (post "Second Post" "This is another post")
        (post "First Post" "This is my first post")))

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

; render-blog-page: blog request -> response
; Consumes a blog request and produces an HTML page of the content of the blog
(define (render-blog-page a-blog request)
  (response/xexpr
   `(html (head (title "My Blog"))
          (body (h1 "My Blog") ,(render-posts a-blog)))))

; start: request -> response
; Consumes a requets and produces a page that displays all the web content
(define (start request)
  (render-blog-page BLOG request))