;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive directory listing
;;
;; In keeping with http://mama.indstate.edu/users/ice/tree/index.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(define (tree file-path)
  (define (get-file-type a-file-path)
    (cond
     [(file-exists? a-file-path) 'file]
     [(directory-exists? a-file-path) 'dir]
     [(link-exists? a-file-path) 'link]
     [else (write a-file-path)]))

  (define (indent level a-string [final? #f])
    (define (make-right-part level [right-part ""])
      (cond
       [(<= level 1) right-part]
       [(> level 1)
        (make-right-part (- level 1) (string-append "│   " right-part))]))

    (cond
     [(<= level 0) a-string]
     [(= level 1)
      (string-append (if final? "└── " "├── ") a-string)]
     [(> level 1)
      (string-append (make-right-part level)
                     (if final? "└── " "├── ")
                     a-string)]))

  (define (tree-rec a-file-path file-type level file-id)
    (cond
     [(eq? 'file file-type)
      (define-values (base name must-be-dir?) (split-path a-file-path))
      (displayln (indent level
                         (path->string name)
                         (if (<= file-id 0) #t #f)))]
     [(eq? 'dir file-type)
      (define-values (base name must-be-dir?) (split-path a-file-path))
      (displayln (indent level
                         (path->string name)
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
