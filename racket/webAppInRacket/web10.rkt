#lang web-server/insta

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blog Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;; Posts
;;;;;;;;;;;;;;;;;;;;;

; Blog post data-structure
; A blog post is title, a body (content) and a list of comments.
(struct post (title body comments) #:mutable)

; post-add-comment! : post comment -> void
; Consumes a post and a comment, adds the comment at the end of the post.
(define (post-add-comment! a-post comment)
  (set-post-comments! a-post (append (post-comments a-post) (list comment))))

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
               (list "Comment1" "Comment2" "Comment3")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blog Bindings & Params
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; can-parse-post?: bindings -> boolean
; Test if bindings for a blog post are provided.
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

; can-parse-comment?: bindings -> boolean
; Test if bindings for a post comment are provided.
(define (can-parse-comment? bindings)
  (and (exists-binding? 'comment bindings)
       (> (string-length (extract-binding/single 'comment bindings)) 0)))

; parse-comment: bindings -> comment
; Consumes a bindings and produce a post comment out of the bindindgs
(define (parse-comment bindings)
  (extract-binding/single 'comment bindings))

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
  `(div ((class "comments")) 
        ,(render-as-itemized-list (map render-comment comments))))

; render-post-without-comments: post blog (handler -> string) -> xexpr
; Consumes a blog post and produce an xexpr fragment
(define (render-post-without-comments a-post a-blog embed/url)
  (local [(define (view-post-handler request)
            (render-post-detail-page a-post a-blog request))]
  `(div ((class "post"))
        (a ((href ,(embed/url view-post-handler))) ,(post-title a-post))
        (p ,(post-body a-post))
        (p ,(number->string (length (post-comments a-post))) " comments"))))

; render-post-with-comments: post -> xexpr
; Consumes a blog post and produce an xexpr fragment
(define (render-post-with-comments a-post)
  `(div ((class "post"))
        (h1 ,(post-title a-post))
        (p ,(post-body a-post))
        ,(render-comments (post-comments a-post))))

; render-posts: (listof post) blog (handler -> string) -> xexpr
; Consume a list of post and produce an xexpr fragment
(define (render-posts posts a-blog embed/url)
  `(div ((class "posts"))
        ,(render-as-itemized-list
          (map 
           (lambda (a-post) (render-post-without-comments a-post a-blog embed/url))
           posts))))

;;;;;;;;;;;;;;;;;;;;;
;; Pages
;;;;;;;;;;;;;;;;;;;;;

; render-confirm-add-comment-page: comment post request -> doesn't return
; Consumes a comment that we intend to add to a post, as well
; as the request. If the user follows through, adds a comment
; and goes back to the display page. Otherwise, goes back to
; the detail page of the post.
(define (render-confirm-add-comment-page a-comment a-post a-blog request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Confirm / Cancel"))
                    (body
                     (h1 "Confirm add of comment")
                     (div
                      (dl (dt (b "Title")) (dd ,(post-title a-post))
                          (dt (b "Body")) (dd ,(post-body a-post))
                          (dt (b "Comment")) (dd ,a-comment)))
                     (div 
                      ; Confirm insert comment
                      (a ((href ,(embed/url confirm-add-handler)))
                         "Ok")
                      " / "
                      ; Cancel insert comment
                      (a ((href ,(embed/url cancel-add-handler)))
                         "Cancel"))))))
          
          ; Add comment to post and route to render-post-detail-page.
          (define (confirm-add-handler a-request)
            (post-add-comment! a-post a-comment)
            (render-post-detail-page a-post a-blog a-request))
          
          ; Doesn't add comment to post and route to render-post-detail-page.
          (define (cancel-add-handler a-request)
            (render-post-detail-page a-post a-blog a-request))]
   
    (send/suspend/dispatch response-generator)))

; render-post-detail-page: post blog request -> doesn't return
; Consumes a post request and produces an HTML page of the content of the post
(define (render-post-detail-page a-post a-blog request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title ,(post-title a-post)))
                    (body 
                     ,(render-post-with-comments a-post)
           
                     ; Form to add a new comment
                     (form ((action ,(embed/url insert-comment-handler)))
                           (textarea ((name "comment")))
                           (br)
                           (input ((type "submit"))))
                     
                     ; Comme back to render-blog-page
                     (a ((href ,(embed/url back-handler))) "Back")))))
          
          ; insert
          (define (insert-comment-handler a-request)
            (let ([bindings (request-bindings a-request)])
              (cond [(can-parse-comment? bindings)
                     (render-confirm-add-comment-page (parse-comment bindings) 
                                                      a-post 
                                                      a-blog
                                                      a-request)])))

          (define (back-handler a-request)
            (render-blog-page a-blog a-request))]
    
    (send/suspend/dispatch response-generator)))

; render-blog-page: blog request -> doesn't return
; Consumes a blog request and produces an HTML page of the content of the blog
(define (render-blog-page a-blog request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "My Blog"))
                    (body (h1 "My Blog")
                          ,(render-posts (blog-posts a-blog) a-blog embed/url)
                          
                          ; Form to add a new blog post
                          (form ((action ,(embed/url insert-post-handler)))
                                (input ((name "title")))(br)
                                (textarea ((name "body")))(br)
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