;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directory iteration.
;; This exercize come from boost filesystem tutorial
;;
;; http://www.boost.org/doc/libs/1_49_0/libs/filesystem/v3/doc/tutorial.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

;; size-of-file: file-path -> size of file
;; Consume a file path and return a string with the size of file. If
;; path is a directory, then the function returns a string with
;; direcoty name and iter on each files. If path is nothing relevant
;; in the filesystem, then the function returns a string expliciting
;; that the path does not exist.
(define (size-of-file file-path)
  (fold-files
   (lambda (a-file-path file-type acc)
     (cond
      [(eq? 'file file-type)
       (printf "~a size is ~a\n" a-file-path (file-size a-file-path))]
      [(eq? 'dir file-type)
       (printf "~a is directory\n" a-file-path)]
      [else
       (printf "~a does not exist\n" a-file-path)])) void))
