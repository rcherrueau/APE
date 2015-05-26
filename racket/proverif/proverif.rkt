#! /usr/bin/env racket
#lang racket/base

(require "fill-rastache/rastache/main.rkt"
         racket/system
         racket/cmdline)

(cond
  [(system "hash proverif 2>/dev/null")
   (define file-to-compile
     (command-line
      #:args (filename)
      filename))
   (define output (open-output-file "compiled-rastache.pv" #:exists 'replace))
   (rast-compile/render (open-input-file file-to-compile) #hash{} output)
   (system "proverif compiled-rastache.pv")]
  [else (displayln "I require proverif but it's not installed.  Aborting.")])
