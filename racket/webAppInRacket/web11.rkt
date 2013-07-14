;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web Applications in Racket
;; http://docs.racket-lang.org/continue/
;; 
;; 11. Decorating With Style!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

; Include style
(static-files-path "style")

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
               "This is another post. Lorem ipsum dolor sit amet, consectetur
                adipisicing elit, sed do eiusmod tempor incididunt ut labore et
                dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
                exercitation ullamco laboris nisi ut aliquip ex ea commodo
                consequat. Duis aute irure dolor in reprehenderit in voluptate
                velit esse cillum dolore eu fugiat nulla pariatur. Excepteur
                sint occaecat cupidatat non proident, sunt in culpa qui officia
                deserunt mollit anim id est laborum."
               (list "Comment1" "Comment2"))
         (post "First Post"
               "This is my first post. Lorem ipsum dolor sit amet, consectetur
                adipisicing elit, sed do eiusmod tempor incididunt ut labore et
                dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
                exercitation ullamco laboris nisi ut aliquip ex ea commodo
                consequat. Duis aute irure dolor in reprehenderit in voluptate
                velit esse cillum dolore eu fugiat nulla pariatur. Excepteur
                sint occaecat cupidatat non proident, sunt in culpa qui officia
                deserunt mollit anim id est laborum."
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
        (h4 ,(post-title a-post))
        (p ((class "palette-paragraph")) ,(post-body a-post))
        (a ((href ,(embed/url view-post-handler))) 
              ,(number->string (length (post-comments a-post))) " comments..."))))

; render-post-with-comments: post -> xexpr
; Consumes a blog post and produce an xexpr fragment
(define (render-post-with-comments a-post)
  `(div ((class "post"))
        (h1 ,(post-title a-post))
        (p ((class "palette-paragraph")) ,(post-body a-post))
        ,(render-comments (post-comments a-post))))

; render-posts: (listof post) blog (handler -> string) -> xexpr
; Consume a list of post and produce an xexpr fragment
(define (render-posts posts a-blog embed/url)
  `(div ((class "posts"))
        ,(render-as-itemized-list
          (map 
           (lambda (a-post) 
             (render-post-without-comments a-post a-blog embed/url))
           posts))))

; render-menu-li: 'active url-tag icon -> xexpr
; Consume element to construct an li for the menu. 
(define (render-menu-li active url-tag icon)
  (cond
    [(eq? 'active active) 
     `(li
       ((class "active"))
       (a ((href ,url-tag))
          (i ((class ,(string-append "fui-" icon))) "")))]
    [else 
     `(li
       (a ((href ,url-tag))
          (i ((class ,(string-append "fui-" icon))) "")))]))

; render-container: a-title (list-of li) content -> xexpr
(define (render-container a-title lis content)
  
  ; render-menu: (list-of xexpr) -> xexpr
  ; Consume a list of li and produce an 
  ; xexpr for the menu
  (define (render-menu lis)
    `(ul ((class "nav nav-list"))
         ,@lis))
  
  `(html (head (title ,a-title)
               (link ((rel "stylesheet")
                      (href "/flat-ui/bootstrap/css/bootstrap.css")
                      (type "text/css")))
               (link ((rel "stylesheet")
                      (href "/flat-ui/css/flat-ui.css")
                      (type "text/css")))
               (link ((rel "stylesheet")
                      (href "/web-racket.css")
                      (type "text/css"))))
         (body
          (div ((class "container"))
               (div ((class "row"))
                    (div ((class "span1 menu")) 
                         ,(render-menu lis))
                    (div ((class "span8 content")) ,content))))))

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
             #:preamble #"<!DOCTYPE html>"
             (render-container
              "Confirm add of comment"
              (list 
               (render-menu-li 'not-active (embed/url back-handler) "cmd")
               (render-menu-li 'not-active "#ViewPost" "eye")
               (render-menu-li 'active "#NewComment" "new"))
              `(div
                (h1  "Confirm add of comment")
                (div
                 (dl (dt (b "Title")) (dd ,(post-title a-post))
                     (dt (b "Body")) (dd ,(post-body a-post))
                     (dt (b "Comment")) (dd ,a-comment)))
                (div 
                 ; Confirm insert comment
                 (a ((href ,(embed/url confirm-add-handler)))
                    (button ((type "submit")
                             (class "span2 btn btn-primary btn-large"))
                            "Confirm"))
                 ; Cancel insert comment
                 (a ((href ,(embed/url cancel-add-handler)))
                    (button ((class "span2 btn btn-large"))  "Cancel")))))))
          
          ; Add comment to post and route to render-post-detail-page.
          (define (confirm-add-handler a-request)
            (post-add-comment! a-post a-comment)
            (render-post-detail-page a-post a-blog a-request))
          
          ; Doesn't add comment to post and route to render-post-detail-page.
          (define (cancel-add-handler a-request)
            (render-post-detail-page a-post a-blog a-request))
          
          ; Come Back to main page
          (define (back-handler request)
            (render-blog-page a-blog request))]
   
    (send/suspend/dispatch response-generator)))

; render-post-detail-page: post blog request -> doesn't return
; Consumes a post request and produces an HTML page of the content of the post
(define (render-post-detail-page a-post a-blog request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             #:preamble #"<!DOCTYPE html>"
             (render-container 
              (post-title a-post)
              (list 
               (render-menu-li 'not-active (embed/url back-handler) "cmd")
               (render-menu-li 'active "#ViewPost" "eye")
               (render-menu-li 'not-active "#NewComment" "new"))
              `(div ,(render-post-with-comments a-post)
                    ; Form to add a new comment
                    (h4 ((id "NewComment")) "New Post")
                    (form 
                     ((action ,(embed/url insert-comment-handler)))
                     (div ((class "controls docs-input-sizes"))
                          (textarea ((name "comment")
                                     (class "span8")
                                     (rows "10")) "")
                          (button ((type "submit")
                                   (class "btn btn-primary btn-large"))
                                  "Publish")
                          ; Comme back to render-blog-page
                          (a ((href ,(embed/url back-handler))
                              (class "btn btn-large"))  
                             "Back")))))))
          
          ; Form action
          (define (insert-comment-handler a-request)
            (let ([bindings (request-bindings a-request)])
              (cond [(can-parse-comment? bindings)
                     (render-confirm-add-comment-page (parse-comment bindings) 
                                                      a-post 
                                                      a-blog
                                                      a-request)])))
          ; Come Back to main page
          (define (back-handler request)
            (render-blog-page a-blog request))]
    
    (send/suspend/dispatch response-generator)))

; render-blog-page: blog request -> doesn't return
; Consumes a blog request and produces an HTML page of the content of the blog
(define (render-blog-page a-blog request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             #:preamble #"<!DOCTYPE html>"
             (render-container 
              "My Blog"
              (list 
               (render-menu-li 'active "#Home" "cmd")
               (render-menu-li 'not-active "#NewPost" "new"))
              `(div 
                (h1 ((id "Home")) "My Blog")
                ,(render-posts (blog-posts a-blog) a-blog embed/url)
                
                ; Form to add a new blog post
                (div ((class "new-post"))
                     (h4 ((id "NewPost")) "New Post")
                     (form 
                      ((action ,(embed/url insert-post-handler)))
                      (div ((class "controls docs-input-sizes"))
                           (input ((type "text")
                                   (name "title")
                                   (class "span8")
                                   (placeholder "Enter title here")))
                           (textarea ((name "body")
                                      (class "span8")
                                      (rows "10")) "")                                   
                           (button
                            ((type "submit")
                             (class "btn btn-large btn-block btn-primary"))
                            "Publish"))))))))
          ; Form action  
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