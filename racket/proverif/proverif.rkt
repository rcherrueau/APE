#! /usr/bin/env racket
#lang racket/base

(require (file "/Users/rcherr12/prog/APE/racket/proverif/fill-rastache/rastache/main.rkt")
         racket/system
         racket/cmdline)

(cond
  ;; Proverif is installed
  [(system "hash proverif 2>/dev/null")
   ;; proverif evaluation namespace
   (define proverif-ns (make-base-namespace))

   ;; Command line
   (define color-mode (make-parameter #t))
   (define debug-mode (make-parameter #f))
   (define with-context (make-parameter #hash()))
   (define file-to-compile
     (command-line
      #:usage-help "Cryptographic protocol verifier"
      #:once-each
      [("-t" "--test")    "display a bit more information for debugging"
                          (debug-mode #t)]
      [("--no-color")     "do not use ANSI color codes"
                          (color-mode #f)]
      [("-c" "--context") context
                          "set the context for mustache expansion"
                          (with-context
                            (eval (read (open-input-string context))
                                  proverif-ns))]
      #:args (filename)
      filename))

   (cond
     ;; The given file exists
     [(file-exists? file-to-compile)
      ;; Compile option
      (define options
        (let ([opts ""])
          (when (color-mode) (set! opts (string-append "-color " opts)))
          (when (debug-mode) (set! opts (string-append "-test " opts)))
          opts))

      ;; Expand rastache file
      (define output-name "compiled-rastache.pv")
      (define output (open-output-file output-name
                                       #:exists 'replace))
      (define tokens (rast-compile/open-file file-to-compile))
      (rast-render tokens (with-context) output)

      ;; Call proverif on expanded file
      (displayln (format "proverif ~a ~s" options output-name))
      (void (system (format "proverif ~a ~s" options output-name)))]

     ;; The given file doesn't exist
     [else
      (error (format "File error: ~s: No such file or directory"
                     file-to-compile))])]

  ;; Proverif is not installed
  [else (error "I require proverif but it's not installed.  Aborting.")])
