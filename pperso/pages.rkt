#lang racket/base

(provide (all-defined-out))

;; List of publications, sorted by type and year.
(define publications
  #hash{
   (articles . [
           #hash{(year . 2018) (articles . [
             #hash{
                   (authors . "Ronan-Alexandre Cherrueau,
                               Adrien Lebre,Dimitri Pertin,
                               Fetahi Wuhib and
                               João Monteiro Soares")
                   (title . "Edge Computing Resource Management
                    System: a Critical Building Block!
                    Initiating the debate via OpenStack")
                   (booktitle . "USENIX Workshop on Hot Topics in
                    Edge Computing &mdash; HotEdge 2018")
                   (address .  "Boston, USA")
                   (month . "July")
                   (pdf-url . "https://hal.archives-ouvertes.fr/hal-01812747/file/hotedge2018.pdf")
                   (url . "https://www.usenix.org/conference/hotedge18/presentation/cherrueau") }
             #hash{
                   (authors . "Ronan-Alexandre Cherrueau,
                               Matthieu Simonin and
                               Alexandre van Kempen")
                   (title . "EnosStack: A LAMP-like stack for
                     the experimenter")
                   (booktitle . "The 5th IEEE INFOCOM Workshop on
                    Computer and Networking Experimental Research
                    using Testbeds &mdash; CNERT 2018")
                   (pages . "336 &mdash; 341")
                   (address .  "Honolulu, HI, USA")
                   (month . "April")
                   (pdf-url . "https://hal.inria.fr/hal-01689726/file/RR-9146.pdf")
                   (url . "http://dx.doi.org/10.1109/INFCOMW.2018.8407024") } ])}
          #hash{(year . 2017) (articles . [
             #hash{
                   (authors . "Ronan-Alexandre Cherrueau,
                               Dimitri Pertin, Anthony Simonet,
                               Adrien Lebre and Matthieu Simonin")
                   (title . "Toward a Holistic Framework for Conducting Scientific Evaluations of OpenStack")
                   (booktitle . "The 17th IEEE/ACM International
                    Symposium on Cluster, Cloud and Grid
                    Computing &mdash; CCGRID 2017")
                   (pages . "544 &mdash; 548")
                   (address . "Madrid, Spain")
                   (month . "May")
                   (pdf-url . "https://hal.archives-ouvertes.fr/hal-01664515/file/main.pdf")
                   (url . "http://dx.doi.org/10.1109/CCGRID.2017.87") }

            ])}
          #hash{(year . 2015) (articles . [
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
                    (pdf-url . "https://hal.archives-ouvertes.fr/hal-01168303/document")
                    (url . "http://dx.doi.org/10.1109/Trustcom.2015.480") } ])}
           #hash{(year . 2014) (articles . [
             #hash{ (authors . "Ronan-Alexandre Cherrueau and
                                Mario Südholt")
                    (title . "Enforcing Expressive Accountability Policies" )
                    (booktitle . "IEEE 23rd International WETICE
                                  Conference &mdash; WETICE 2014")
                    (pages . "519 &mdash; 524")
                    (address . "Parma, Italy")
                    (month . "June")
                    (url . "http://dx.doi.org/10.1109/WETICE.2014.71") } ])}
           #hash{(year . 2013) (articles . [
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
                    (url . "http://doi.acm.org/10.1145/2451617.2451621") }])}])

   (reports  . [ ])

   (thesis   .

             #hash{ (authors . "Ronan-Alexandre Cherrueau")
                    (title . "A Compositional Language of Security
                              Techniques for Information Privacy in
                              the Cloud" )
                    (school . "École des Mines de Nantes")
                    (defense . "November 2016")
                    (pdf-url . "assets/phd/manuscript.pdf")
                    (slides-url . "assets/phd/slides.pdf")
                    (keywords . "Privacy, information privacy,
                                 cloud computing, encryption,
                                 data fragmentation, client-side
                                 computation, programming language,
                                 Idris, algebraic laws, automatique
                                 verifivation, ProVerif")
                    (url . "https://tel.archives-ouvertes.fr/tel-01416166") }
             ) })

;; A web page (and its constructor).
;;
;; name: name of the page, also in the title
;; url: relative url of the page
;; cmd: command to print into prompt
;; content: filepath of the page content
;; title?: print `name` in the title
;; other : Other informations.
(struct page (name url cmd template title? other) #:transparent)
(define (p name url cmd template #:title [title? #t]
           #:other [other #f])
  (page name url cmd template title? other))

;; List of all pages.
(define pages
  (list (p "Ronan-Alexandre Cherrueau"
           "index.html"
           "about"
           "templates/mu-index.html"
           #:title #f)
        (p "Publications"
           "publications.html"
           "publications"
           "templates/mu-publi.html"
           #:other publications)
        ;; (p "Projects"
        ;;    "projects.html"
        ;;    "projects"
        ;;    "templates/mu-proj.html")
        ;; (p "OpenStack In a Coconut"
        ;;    "os-coconut.html"
        ;;    "openstack | coconut"
        ;;    "templates/mu-os-coconut.html")
        ;; (p "Teaching"
        ;;    "teaching.html"
        ;;    "teaching"
        ;;    "templates/mu-teaching.html")
        ))
