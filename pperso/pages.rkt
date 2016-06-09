#lang racket/base

(provide (all-defined-out))

;; A web page (and its constructor).
;;
;; name: name of the page, also in the title
;; url: relative url of the page
;; cmd: command to print into prompt
;; content: filepath of the page content
;; title?: print `name` in the title
;; other : Other informations.
(struct page (name url cmd content title? other) #:transparent)
(define (p name url cmd content #:title [title? #t]
                                #:other [other #f])
  (page name url cmd content title? other))

;; List of publications, sorted by type and year.
(define publications
  #hash{
   (articles .
    #hash{ (a2015 .
            [
             #hash{ (authors . "Ronan-Alexandre Cherrueau,
                    Rémi Douence and
                    Mario Südholt")
                    (title . "A Language for the Composition of
                  Privacy-Enforcement Techniques")
                    (booktitle . "IEEE International Symposium on
                      Recent Advances of Trust, Security and
                      Privacy in Computing and Communications
                      &mdash; RATSP 2015")
                    (pages . "519 &mdash; 524")
                    (address . "Helsinki, Finland")
                    (month . "August")
                    (url . "http://dx.doi.org/10.1109/Trustcom.2015.480") } ])
           (a2014 .
            [
             #hash{ (authors . "Ronan-Alexandre Cherrueau and
                                Mario Südholt")
                    (title . "Enforcing Expressive Accountability Policies" )
                    (booktitle . "IEEE 23rd International WETICE
                                  Conference &mdash; WETICE 2014")
                    (pages . "519 &mdash; 524")
                    (address . "Parma, Italy")
                    (month . "June")
                    (url . "http://dx.doi.org/10.1109/WETICE.2014.71") } ])
           (a2013 .
            [
             #hash{ (authors . "Ronan-Alexandre Cherrueau,
                                Mario Südholt and
                                Omar Chebaro")
                    (title . "Adapting Workflows Using Generic
                              Schemas: Application to the Security
                              of Business Processes" )
                    (booktitle . "IEEE 5th International Conference on
                                  Cloud Computing Technology and Science
                                  &mdash; CloudCom 2013")
                    (pages . "519 &mdash; 524")
                    (address . "Bristol, UK")
                    (month . "December")
                    (url . "http://dx.doi.org/10.1109/CloudCom.2013.75") }
             #hash{ (authors . "Ronan-Alexandre Cherrueau,
                                Rémi Douence,
                                Jean-Claude Royer,
                                Mario Südholt,
                                Anderson Santana de Oliveira,
                                Yves Roudier and
                                Matteo Dell'Amico")
                    (title . "Reference Monitors for Security and
                              Interoperability in OAuth 2.0")
                    (booktitle . "Data Privacy Management and Autonomous
                                  Spontaneous Security - 8th
                                  International Workshop, DPM 2013,
                                  and 6th International Workshop,
                                  SETOP 2013")
                    (pages . "235 &mdash; 249")
                    (address . "Egham, UK")
                    (month . "September")
                    (url . "http://dx.doi.org/10.1007/978-3-642-54568-9_15") }
             #hash{ (authors . "Ronan-Alexandre Cherrueau,
                                Omar Chebaro and
                                Mario Südholt")
                    (title . "Flexible Aspect-Based Service Adaptation for
                              Accountability Properties in the Cloud" )
                    (booktitle . "4th International Workshop on
                                  Variability & Composition
                                  &mdash; VariComp 2013")
                    (pages . "13 &mdash; 18")
                    (address . "Fukuoka, Japan")
                    (month . "March")
                    (url . "http://doi.acm.org/10.1145/2451617.2451621") }]) })
   (reports  . [ ])
   (thesis   . #f) })

;; List of all pages.
(define pages
  (list (p "Ronan-Alexandre Cherrueau"
           "index.html"
           "about"
           "rsc/index.html"
           #:title #f
           #:other
           publications)
        (p "Publications"
           "publications.html"
           "publications"
           "rsc/publi.html"
           #:other
           publications)
        (p "Projects"
           "projects.html"
           "projects"
           "rsc/proj.html")
        ;; (p "Teaching"
        ;;    "teaching.html"
        ;;    "teaching"
        ;;    "rsc/teaching.html")
        ))
