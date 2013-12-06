#lang racket

; (struct var-wiki (articles, categories) #:mutable)
; (struct var-article (title, date, content, category, path) #:mutable)

(define (wiki-structure wiki-path)
  (define wiki list)
  (fold-files
   (lambda (a-path)
     (cond [(is-article? a-path)
            (define an-article (create-article a-path))
            (cons wiki an-article)]
           [(is-category? a-path)
            (cons wiki (create-category a-path))]))
   wiki wiki-path))
