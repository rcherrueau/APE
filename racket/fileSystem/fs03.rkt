;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive directory listing
;;
;; In keeping with http://mama.indstate.edu/users/ice/tree/index.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

;; tree: file-path -> void
;; Recursive directory listing.
(define (tree file-path)

  (define (levels-add-not-final levels)
    (append levels (list 'not-final)))

  (define (levels-add-final levels)
    (append levels (list 'final)))

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
     [(link-exists? a-file-path) 'link]))

  ;; indent: level file-path-name level final-level [final?] ->
  ;;                                           indented-file-path-name
  ;; Indent file-path-name with the correct level of indentation. If
  ;; file is the last of it's sub-tree (final? to #t), the indentation
  ;; system use specific character. level is the level of indentation.
  ;; final-level is the level of final indentation.
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

  ;; tree-rec: a-file-path file-ype level file-id -> void
  ;; Recursive directory listing.
  (define (tree-rec a-file-path file-type levels file-id)
    (define final? (<= file-id 0))

    (cond
     [(eq? 'file file-type)
      (define-values (base name must-be-dir?) (split-path a-file-path))
      (displayln (indent (path->string name) levels final?))]
     [(eq? 'dir file-type)
      (define-values (base name must-be-dir?) (split-path a-file-path))
      (displayln (indent (path->string name) levels final?))

      (define sub-file-paths (directory-list a-file-path))
      (define sub-file-paths-ids (reverse (range (length sub-file-paths))))
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
                empty
                (first file-ids))

      (rest file-ids))

    (void (foldl directory-listing sub-file-paths-ids sub-file-paths))]))
