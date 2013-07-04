#lang web-server/insta

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blog Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;; Posts
;;;;;;;;;;;;;;;;;;;;;

; Blog post data-structure
; A blog post is title, a body (content) and a list of comments.
(struct post(title body comments) #:mutable)

; post-add-comment! : post comment -> void
; Consumes a post and a comment, adds the comment at the end of the post.
(define (post-add-comment! post comment)
  (set-post-comments! (append (post-comments post) (list comment))))

;;;;;;;;;;;;;;;;;;;;;
;; Blog
;;;;;;;;;;;;;;;;;;;;;

; A blog is a (blog posts) where posts is a (listof post)
; Mutable structure provide mutators to change their filds.
; So blog struct provide value set-blog-posts! : blog (listof post) -> void to
; set the posts value.
(struct blog (posts) #:mutable)

; blog-insert-post!: blog post -> void
; Consumes a blog and a post, adds the pot at the top of the blog.
(define (blog-insert-post! a-blog a-post)
  (set-blog-posts! a-blog
                   (cons a-post (blog-posts a-blog))))

; BLOG: blog
; The initial BLOG.
(define BLOG 
  (blog 
   (list (post "Second Post"
               "This is another post"
               (list "Comment1" "Comment2"))
         (post "First Post"
               "This is my first post"
               (list "Comment1" "Comment2")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blog Bindings & Params
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; can-parse-post?: bindings -> boolean
; Test if bindings for a blog post are provided
; When creating a blog post, the list of comment is obviously empty
; so there is no test on comments argument.
(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
       (> (string-length (extract-binding/single 'title bindings)) 0)
       (exists-binding? 'body bindings)
       (> (string-length (extract-binding/single 'body bindings)) 0)))

; parse-post: bindings -> post
; Consumes a bindings and produce a blog post out of the bindings
; When you create a blog-post, list of comments is obviously empty.
(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)
        (list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blog Render Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-comment: comment -> xexpr
; Consumes a post comment and produce an xexpr fragment
(define (render-comment comment)
  `(div ((class "comment")) (p ,comment)))


; render-comments: (listof comment) -> xexpr
; Consume a list of post comment and produce an xexpr fragment
(define (render-comments comments)
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

  `(div ((class "comments")) 
        ,(render-as-itemized-list (map render-comment comments))))

; render-post: post -> xexpr
; Consumes a blog post and produce an xexpr fragment
(define (render-post post)
  `(div ((class "post"))
        ,(post-title post)
        (p ,(post-body post))
        ,(render-comments (post-comments post))))

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
           S  `(html (head (title "My Blog"))
                    (body (h1 "My Blog")
                          ,(render-posts (blog-posts a-blog))
                          (form ((action ,(embed/url insert-post-handler)))
                                (input ((name "title")))
                                (input ((name "body")))
                                (input ((type "submit"))))))))
          (define (insert-post-handler request)
            ((let ([bindings (request-bindings request)])
               (cond [(can-parse-post? bindings)
                      (blog-insert-post! a-blog (parse-post bindings))]))
            (render-blog-page a-blog request)))]
  (send/suspend/dispatch response-generator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; start: request -> response
; Consumes a requets and produces a page that displays all the web content
(define (start request)
  (render-blog-page BLOG request))