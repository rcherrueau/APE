;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web Applications in Racket
;; http://docs.racket-lang.org/continue/
;; 
;; 13. Abstracting the Model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket/base


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

; Grant other files access to eveything defined in this file. 
(provide (all-defined-out))