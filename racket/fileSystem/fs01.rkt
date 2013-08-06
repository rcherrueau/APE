;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reporting the size of a file
;; This exercize comes from boost filesystem tutorial
;;
;; http://www.boost.org/doc/libs/1_49_0/libs/filesystem/v3/doc/tutorial.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

;; size-of-file: file-path -> size of file
;; Consume a file path and return a string with the size of file. If
;; path is a directory, then the function returns a string with
;; direcoty name. If path is nothing relevant in the filesystem, then
;; the function returns a string expliciting that the path does not
;; exist.
(define (size-of-file file-path)
  (cond
   [(file-exists? file-path)
    (format "~a size is ~a" file-path (file-size file-path))]
   [(directory-exists? file-path)
    (format "~a is directory" file-path)]
   [else
    (format "~a does not exist" file-path)]))
