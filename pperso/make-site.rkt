#! /usr/bin/env racket
;; Generator for my personal homepage
#lang racket/base

(require rastache    ;; https://github.com/rcherrueau/rastache
         racket/file
         racket/port
         racket/cmdline)

;; The mustache page template.
(define template "mustache.html")

;; A web page (and its constructor).
;;
;; name: name of the page, also in the title
;; url: relative url of the page
;; cmd: command to print into prompt
;; content: filepath of the page content
;; title: print `name` in the title
(struct page (name url cmd content title?) #:transparent)
(define (p name url cmd content [title? #t])
  (page name url cmd content title?))

;; List of all pages.
(define pages
  (list (p "Ronan-Alexandre Cherrueau"
           "index.html"
           "about"
           "rsc/index.html"
           #f)
        (p "Projects"
           "projects.html"
           "projects"
           "rsc/proj.html")
        (p "Publications"
           "publications.html"
           "publications"
           "rsc/publi.html")
        (p "Teaching"
           "teaching.html"
           "teaching"
           "rsc/teaching.html")))

;; The web page Mustache template compiled.
(define tokens (make-parameter (rast-compile/open-file template)))


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

;; Generates a specific page.
(define (make-page page)
  (call-with-output-file (page-url page) #:exists 'replace
    (λ (ostream)
      (rast-render (tokens) (make-ctx page) ostream))))

;; Generates all pages of the web site.
(define (make-pages)
  (for-each make-page pages))

;; Deamon to continuously update the generation.
(define (daemon)
  (define main-cust (make-custodian))

  ;; When template changes: recompiles tokens and re-generates all
  ;; pages
  (define (handle-tplt)
    (parameterize ([tokens (rast-compile/open-file template)])
      (make-pages)
      (displayln "Regenerated")))

  ;; When content changes: regenerate the page.
  (define (handle-page page)
    (make-page page)
    (displayln (format "~s Regenerated" (page-url page))))

  ;; Observe files
  (parameterize ([current-custodian main-cust])
    (thread (λ ()
              (let loop ()
                (sync (filesystem-change-evt template))
                (handle-tplt)
                (loop))))

    (for-each (λ (p)
                (thread (λ ()
                          (let loop ()
                            (sync (filesystem-change-evt (page-content p)))
                            (handle-page p)
                            (loop)))))
              pages))

  ;; Shutdown on Ctrl-D
  (displayln "Press Ctrl-D to quit")
  (sync (eof-evt (current-input-port)))
  (custodian-shutdown-all main-cust)
  (displayln "Shutdown"))

;; Main program
(module* main #f
  (define daemon? (make-parameter #f))
  (command-line
   #:usage-help "Generator for my personal webpage"
   #:once-any [("-d" "--daemon") "continuously update" (daemon? #t)])

  (cond
    [(daemon?) (daemon)]
    [else (make-pages)]))
