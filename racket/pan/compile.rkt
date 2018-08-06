#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline
         racket/system
         racket/path)

;; Execute a shell command
(define (exec cmd)
  (displayln cmd)
  (system cmd))

;; Get the name of the file to compile
(define executable-name (make-parameter #f))
(define file-to-compile
  (command-line
   #:usage-help "Compile <filename>"
   #:once-each
   [("-o" "--output") file "Place the output into <file>"
                      (executable-name file)]
   #:args (filename)
   filename))

;; Produce the asm file
(define asm-output-name "_the_asm_code.s")
(with-output-to-file asm-output-name #:exists 'replace
  ;; Dynamically load the file to compile. The dynamic loading should
  ;; produce the asm code on stdout.
  (λ () (dynamic-require file-to-compile #f)))

;; Compile the whole stuff:
;; > as --32 -o <asm-object-name> _the_asm_code.s
(define asm-object-name (path-replace-extension asm-output-name ".o"))
(define asm-cmd (format "as --32 -o ~a ~a" asm-object-name asm-output-name))
(exec asm-cmd)

;; > gcc -m32 -o <executable-name> _the_asm_code.o _main.c
(define gcc-cmd (format "gcc -m32 -o ~a ~a _main.c"
                        (or (executable-name)
                            (path-replace-extension file-to-compile ""))
                        asm-object-name))
(exec gcc-cmd)
