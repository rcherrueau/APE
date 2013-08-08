;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive directory listing
;;
;; In keeping with http://mama.indstate.edu/users/ice/tree/index.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

;; tree: file-path -> void
;; Recursive directory listing.
(define (tree file-path)

  ;; get-file-type: a-file-path -> file-type
  ;; Consumes a file path and return the type of file.
  ;; 'file if file-path targets a file
  ;; 'dire if file-paht targets a directory
  ;; 'link if file-path targets a link
  (define (get-file-type a-file-path)
    (cond
     [(file-exists? a-file-path) 'file]
     [(directory-exists? a-file-path) 'dir]
     [(link-exists? a-file-path) 'link]))

  ;; indent: level file-path-name level [final?] -> indented-file-path-name
  ;; Indent file-path-name with the correct level of indentation. If
  ;; file is the last of it's sub-tree (final? to #t), the indentation
  ;; system use specific character.
  (define (indent file-path-name level [final? #f])
    (define (make-right-part level [right-part ""])
      (cond
       [(<= level 1) right-part]
       [(> level 1)
        (make-right-part (- level 1) (string-append "│   " right-part))]))

    (cond
     [(<= level 0) file-path-name]
     [(= level 1)
      (string-append (if final? "└── " "├── ") file-path-name)]
     [(> level 1)
      (string-append (make-right-part level)
                     (if final? "└── " "├── ")
                     file-path-name)]))

  ;; tree-rec: a-file-path file-ype level file-id -> void
  ;; Recursive directory listing.
  (define (tree-rec a-file-path file-type level file-id)
    (cond
     [(eq? 'file file-type)
      (define-values (base name must-be-dir?) (split-path a-file-path))
      (displayln (indent (path->string name)
                         level
                         (if (<= file-id 0) #t #f)))]
     [(eq? 'dir file-type)
      (define-values (base name must-be-dir?) (split-path a-file-path))
      (displayln (indent (path->string name)
                         level
                         (if (<= file-id 0) #t #f)))

      (define sub-file-paths (directory-list a-file-path))
      (define sub-file-paths-ids (reverse (range (length sub-file-paths))))
      (define (directory-listing sub-file-path file-ids)
        (define new-file-path (build-path a-file-path sub-file-path))

        (tree-rec new-file-path
                  (get-file-type new-file-path)
                  (add1 level)
                  (first file-ids))

        (rest file-ids))

      (foldl directory-listing sub-file-paths-ids sub-file-paths)]))


  (define file-type (get-file-type file-path))
  (cond
   [(eq? 'file file-type)
    (displayln file-path)]
   [(eq? 'dir file-type)
    (displayln file-path)

    (define sub-file-paths (directory-list file-path))
    (define sub-file-paths-ids (reverse (range (length sub-file-paths))))
    (define (directory-listing a-file-path file-ids)
      (define new-file-path (build-path file-path a-file-path))

      (tree-rec new-file-path
                (get-file-type new-file-path)
                1
                (first file-ids))

      (rest file-ids))

    (void (foldl directory-listing sub-file-paths-ids sub-file-paths))]))
