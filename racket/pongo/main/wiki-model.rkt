#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Pattern for a well formed article file path: YYYY-MM-DD-title.MARKUP
(define article-file-path-pattern
  (pregexp "(\\d{4}-\\d{2}-\\d{2})-(.+)?\\..+"))

; Get file name from the file path
(define (file-name file-path)
  (cond [(path? file-path)
         (define-values (base name _) (split-path file-path))
         (path->string name)]
        [else file-path]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Category
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Wiki category data-strcture
; A wiki category is a title, a list of articles and a ath from the
; wiki main directory.
(struct category (title contents path))

; is-category?: file-path -> boolean
; Test if the current file path is a wiki category.
(define (is-category? file-path)
  (directory-exists? file-path))

; create-category: file-path -> category
; Create a category from a file path. At creation, the content list is
; empty.
(define (create-category file-path)
  (define-values (base name _) (split-path file-path))
  (category ((path->string name) empty file-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artcile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Wiki article data-structure.
; A wiki article is a title, a date, the generated content, the category
; of parent and the path from main directory.
(struct article (title date content category-title path))

; is-article?: file-path -> boolean
; Test if the current file is an article.
(define (is-article? file-path)
  ; Pongo requires articles to be named as
  ; YYYY-MM-DD-title.MARKUP
  (define (is-well-formed? a-file-path)
    (regexp-match-exact? article-file-path-pattern (file-name a-file-path)))

  (and (file-exists? file-path)
       (is-well-formed? file-path)))

; create-article: file-path (article-content -> generated-article) -> article
; Create a wiki article from a file-path. The function uses a processos
; to generate the article content from the markup language.
(define (create-article file-path processor)
  (define-values (base name _) (split-path file-path))
  (define-values (parent-base parent-name __) (split-path base))
  (define-values (___ date title) (regexp-match article-file-path-pattern name))
  (define content (file->string file-path))

  (article (title date (processor content) parent-name file-path)))
