#! /usr/bin/env racket
;; Generator for my personal homepage
#lang racket/base

(require "pages.rkt"     ;; Provides `pages`
         rastache        ;; https://github.com/rcherrueau/rastache
         racket/file
         racket/port
         racket/cmdline
         racket/match
         net/url-string
         net/git-checkout)

;; The mustache page template.
(define template "mu-scaffolding.html")

;; The output folder (output of the compilation)
(define output-path (build-path (current-directory )"out"))

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

  (define is-publi-page? (equal? (page-url page) "publications.html"))

  (define ctx
  `#hash{ (name         . ,(page-name page))
          (title        . ,(page-title? page))
          (sections     . ,sections)
          (cmd          . ,(page-cmd page))
          (publications . ,(if is-publi-page? (page-other page) #f))
          (debug        . ,(λ (ctx render) (displayln ctx))) })

  (define ctx/content
    (hash-set ctx
              'content
              (call-with-output-string
               (λ (ostream)
                 (let ([tokens (rast-compile/open-file (page-template page))])
                   (rast-render tokens ctx ostream))))))

  ctx/content)

;; Generates a specific page and returns the url of the generated
;; page.
;; page -> generated-page-path
(define (make-page page)
  (define output-file-path (build-path output-path (page-url page)))
  (call-with-output-file output-file-path #:exists 'replace
    (λ (ostream)
      (rast-render (tokens) (make-ctx page) ostream)))

  (define file-url (url->string (path->url output-file-path)))
  (displayln (format "Generation of ~s" file-url))
  output-file-path)

;; Generates all pages of the web site and returns a list of
;; generated pages.
;; () -> '(page-path)
(define (make-pages) (map make-page pages))

;; Main program
(module* main #f
  ;; -------------------------------------------------- cmd-line
  (define continuous?  (make-parameter #f))
  (define publish?  (make-parameter #f))
  (command-line
   #:program "Personal webpages generator"
   #:once-any
   [("-c" "--continuous") "Continuously update of the webpages"
                          (continuous? #t)]
   [("-p" "--publish") "TODO: Publish webpages"
                       (publish? #t)]
   )

  ;; ------------------------------------------------------ main
  (cond
    [(continuous?)
     (define main-cust (make-custodian))

     (make-pages)

     ;; When template changes: recompiles tokens and re-generates all
     ;; pages
     (define (handle-tplt)
       (parameterize ([tokens (rast-compile/open-file template)])
         (make-pages)))

     ;; When content changes: regenerate the page.
     (define (handle-page page)
       (make-page page))

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
                               (sync (filesystem-change-evt (page-template p)))
                               (handle-page p)
                               (loop)))))
                 pages))

     ;; Shutdown on Ctrl-D
     (displayln "Press Ctrl-D to quit")
     (sync (eof-evt (current-input-port)))
     (custodian-shutdown-all main-cust)
     (displayln "Bye")]
    [(publish?)
     ;; TODO: implement the automatic publishing of the website
     (git-checkout "github.com"
                   "rcherrueau/rcherrueau.github.com.git"
                   #:transport 'git
                   #:dest-dir (find-system-path 'temp-dir))
     ]
    [else (exit (make-pages))]))
