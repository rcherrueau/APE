;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive directory listing
;;
;; In keeping with http://mama.indstate.edu/users/ice/tree/index.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

;; tree: file-path -> void
;; Recursive directory listing.
(define (tree file-path)

  ;; Levels indicates the structure of parents. For each parent, level
  ;; mention if parent is the last one at its depth ('final) or not
  ;; ('not-final). In the following directory listing levels are
  ;; represented at each depths:
  ;;
  ;; ()
  ;; ../../racket
  ;; (final)
  ;;     ├── fileSystem
  ;; (final not-final)
  ;;     │   ├── fs01.rkt
  ;;    ...
  ;; (final not-final)
  ;;     │   └── fs03.rkt
  ;; (final)
  ;;     └── webAppInRacket
  ;; (final final)
  ;;         ├── compiled
  ;; (final final not-final)
  ;;         │   └── drracket
  ;; (final final not-final final)
  ;;         │       └── errortrace
  ;; (final final not-final final final)
  ;;         │           ├── model_rkt.dep
  ;; (final final not-final final final)
  ;;         │           └── model_rkt.zo
  ;; (final final)
  ;;         ├── model.rkt
  ;;        ...
  ;; (final final)
  ;;         └── web13.rkt

  ;; levels-add-not-final: (listof level) -> (listof level)
  ;; Add a not final level to the levels list.
  (define (levels-add-not-final levels)
    (append levels (list 'not-final)))

  ;; levels-add-final: (listof level) -> (listof level)
  ;; Add a final level to the levels list.
  (define (levels-add-final levels)
    (append levels (list 'final)))

  ;; final-level?: level -> boolean
  ;; Test if the given level is final or not.
  (define (final-level? level)
    (eq? 'final level))

  ;; get-file-type: a-file-path -> file-type
  ;; Consumes a file path and return the type of file.
  ;; 'file if file-path targets a file
  ;; 'dir if file-paht targets a directory
  ;; 'link if file-path targets a link
  (define (get-file-type a-file-path)
    (cond
     [(file-exists? a-file-path) 'file]
     [(directory-exists? a-file-path) 'dir]
     [(link-exists? a-file-path) 'link]
     [else 'not-a-file]))

  ;; indent: level file-path-name levels [final?] -> indented-file-path-name
  ;; Indents file-path-name with the correct level of indentation. If
  ;; file is the last of its sub-tree (final? to #t), the indentation
  ;; system use specific character. To indent correctly a file path,
  ;; the system uses a structure (levels) which indicates for each
  ;; parents if the parent is the last one at its depth or not.
  (define (indent file-path-name levels [final? #f])
    (define (make-right-part levels [right-part ""])
      (cond
       [(empty? levels) right-part]
       [(final-level? (first levels))
        (make-right-part (rest levels) (string-append right-part "    " ))]
       [else
        (make-right-part (rest levels) (string-append right-part "│   " ))]))

    (cond
     [(empty? levels) file-path-name]
     [else
      (string-append (make-right-part levels)
                     (if final? "└── " "├── ")
                     file-path-name)]))

  ;; tree-rec: a-file-path file-ype levels file-id [print-base?] -> void
  ;; Recursive directory listing. Takes the current file path to
  ;; print, the type of file (one of the 'file, 'dir, 'link), the
  ;; strucutre of its parents (final or not) and the id of the file in
  ;; its sub-tree (0 is for the last file of its sub tree).
  (define (tree-rec a-file-path file-type levels file-id [print-base? #f])
    (define-values (base name _) (split-path a-file-path))
    (define file-path-to-print
      (cond
       [(not (path? name)) a-file-path]
       [print-base? (string-append (path->string base) (path->string name))]
       [else (path->string name)]))
    (define final? (<= file-id 0))
    (displayln (indent file-path-to-print levels final?))

    (cond
     [(eq? 'dir file-type)
      (define sub-file-paths (directory-list a-file-path))
      (define sub-file-paths-ids (reverse (range (length sub-file-paths))))

      ;; directory-listing: sub-file-path (listof file-id) -> (listof
      ;; file-id) Takes a file path and launch the directory listing
      ;; under this file path. file-ids indicates the position of
      ;; current file path regarding to its brothers. Position of
      ;; current file path is the first element of file-ids list. For
      ;; the last brother of a sub directory, file-ids would be a list
      ;; with only the value 0.
      (define (directory-listing sub-file-path file-ids)
        (define new-file-path (build-path a-file-path sub-file-path))

        (tree-rec new-file-path
                  (get-file-type new-file-path)
                  (if final?
                      (levels-add-final levels)
                      (levels-add-not-final levels))
                  (first file-ids))
        (rest file-ids))

      (void (foldl directory-listing sub-file-paths-ids sub-file-paths))]))

  (define file-type (get-file-type file-path))
  (define print-base #t)
  (define levels empty)
  (define file-id 0)
  (if (eq? file-type 'not-a-file)
      (displayln (string-append file-path " is not a file or directory"))
      (tree-rec file-path file-type levels file-id print-base)))
