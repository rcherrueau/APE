#! /usr/bin/env racket
;; Generator for my personal homepage
#lang racket/base

(require rastache ;; https://github.com/rcherrueau/rastache
         racket/file)

;; A web page (and its constructor).
;;
;; name: name of the page, also in the title
;; url: relative url of the page
;; cmd: command to print into prompt
;; content: filepath of the page content
;; title: print `name` in the title
(struct page (name url cmd content title?) #:transparent)
(define (mkpage name url cmd content [title? #t])
  (page name url cmd content title?))

;; List of all pages.
(define pages
  (list (mkpage "Ronan-Alexandre Cherrueau"
                "index.html"
                "about"
                "rsc/index.html"
                #f)
        (mkpage "Projects"
                "projects.html"
                "projects"
                "rsc/proj.html")
        (mkpage "Publications"
                "publications.html"
                "publications"
                "rsc/publi.html")
        (mkpage "Teaching"
                "teaching.html"
                "teaching"
                "rsc/teaching.html")))

;; The web page Mustache template compiled.
(define tokens (rast-compile/open-file "mustache.html"))

;; Constructs the mustache context for a specific page.
;; page -> ctx
(define (make-ctx page)
  (define sections
    (map (λ (p)
           `#hash{ (name   . ,(page-name p))
                   (url    . ,(page-url p))
                   (active . ,(if (equal? p page) #t #f)) })
         pages))

  `#hash{ (name     . ,(page-name page))
          (title    . ,(page-title? page))
          (sections . ,sections)
          (cmd      . ,(page-cmd page))
          (content  . ,(file->string (page-content page))) })

;; Main
(module* main #f
  ;; Generates all pages of the web site.
  (for-each (λ (page)
              (call-with-output-file (page-url page) #:exists 'replace
                (λ (ostream)
                  (rast-render tokens (make-ctx page) ostream))))
            pages))
