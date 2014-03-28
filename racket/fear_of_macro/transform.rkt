#lang racket

; Fear of Macros
; http://www.greghendershott.com/fear-of-macros/

; What is a syntax transformer? --------------------------------------

; `define-syntax' makes a transformer binding. It tells the Racket
; compiler, "Whenever you encounter a chunck of syntax starting with
; foo, pleasegive it to my transformer function and replace it with
; the syntax I give back to you."
(define-syntax foo
  (lambda (stx) #'"I am foo"))

(foo)

; shortand that lets avoid typing lambda and some parentheses.
(define-syntax (also-foo stx)
  #'"I am also foo")

(also-foo)


(define-syntax (say-hi stx)
  #'(displayln "hi"))

; When Racket expands our program, is sees the occurence of
; `(say-hi)', and sees it has a transformer function for that. It
; calls our function wuth the old syntax, and we return the new
; syntax, which is used to evaluate and run the program.
(say-hi)

; What is the input? -------------------------------------------------

; The `print' shows what our transformer is given: a syntax object.
(define-syntax (show-me stx)
  (print stx)
  #'(void))

; #<syntax:transform.rkt:48:0 (show-me (quote (+ 1 2)))>

; A syntax object constists of several things. The first part is the
; S-expression representing the code, such as '(+ 1 2). The syntax
; object is also decorated with some interesting information such as
; the source file, the line number and column. Finally, it has
; information about lexical.
(show-me '(+ 1 2))

; There are a variety of functions available to access a syntax
; object. Let's define a piece of syntax:
(define stx #'(if x (list "true") #f))
(syntax-source stx)
(syntax-line stx)
(syntax-column stx)

; `syntax->datum' converts the syntax-object into an S-expression.
; '(if x (list "true") #f)
; http://en.wikipedia.org/wiki/S-expression
(syntax->datum stx)

; Whereas `syntax-e' only goes "one level down". It may return a list
; that has syntax objects. Each of those syntax object could be
; converted by `syntax-e', and so on recursively -- which is what
; `syntax->datum' does.
; (#<syntax:transform.rkt:52:15 if>
;  #<syntax:transform.rkt:52:18 x>
;  #<syntax:transform.rkt:52:20 (list "true")>
;  #<syntax:transform.rkt:52:34 #f>)
(syntax-e stx)

; Actually transforming the input ------------------------------------

; transformer that reverses the syntax it was given
(define-syntax (reverse-me stx)
  ; 1. Take the input syntax and give it to `syntax->datum'. This
  ; converts the syntax into a plain old list:
  ; `'(reverse-me "backward" "am" "I" values)'
  ; 2. Using `cdr' slices off the first item of the list,
  ; `reverse-me', leaving the remainder: `'("backward" "am" "I" ;
  ; values)'. And passing that to `reverse' to changes it to
  ; `'(values "I" "am" "backward")'
  ; 3. Finally we use `datum->syntax to convert this back to syntax:
  ; `#<syntax (values "I" "am" "backward")>
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

(reverse-me "backward" "am" "I" values)

; Compile time vs. run time ------------------------------------------

; Normal Racket code run at ... run time. But a syntax transformer is
; called by Racket as part of the process of parsing, expanding and
; compiling our program. In other words, our syntax transformer
; function is evaluated at compile time.
(define-syntax (foo2 stx)
  (make-pipe) ; This is not run time
  #'(void))
(foo2)

; This aspect of macros lets you do things that simply aren't possible
; in normal code. One of the classic examples is something like the
; Racket form `if'. If we implemented `if' as a function, all of the
; arguments will be evaluated before being provided to the function.
(define (my-if condition? true-expr false-expr)
  (cond [condition? true-expr]
        [else false-expr]))
; That seems to work. But, because the expression have a side-effect,
; it's obvious that they are both evaluated. And that could be a
; problem -- what if the side effect includes deleting a file on disk?
; You wouldn't want
; `(if user-wants-file-deleted? (delete-file) (void))'
; to delete a file even when `user-want-file-deleted?' is #f.
(my-if #t
       ((lambda () (displayln "true") "true"))
       ((lambda () (displayln "false") "false")))

; So the if simply can't work as a plain function. However a syntax
; transformer can rearrange the syntax -- rewrite the code -- at
; compile time. The pieces of syntax are moved around, but they aren't
; actually evaluated until runtime.
(define-syntax (my-if2 stx)
  ; (#<syntax::: if>
  ;  #<syntax::: condition?>
  ;  #<syntax::: true-exp>
  ;  #<syntax::: false-exp>)
  (define xs (syntax->list stx))
  (define condition? (cadr xs))
  (define true-exp (caddr xs))
  (define false-exp (cadddr xs))
  (datum->syntax stx `(cond [,condition? ,true-exp]
                            [else ,false-exp])))
(my-if2 #t
       ((lambda () (displayln "true") "true"))
       ((lambda () (displayln "false") "false")))

; And you can do the same with `match'. Using that would let you do
; pattern-matching. But be careful, Racket will complaining that
; `match' isn't defined. Our transformer function is working at
; compile time, not run time. *And at compile time, only racket/base
; is required for you automatically*. Anything beyound racket/base, we
; have to require ourselves -- and require it for compile time using
; the `for-syntax' form of `require'.
(require (for-syntax racket/match))

(define-syntax (my-if-with-match stx)
  (match (syntax->list stx)
    [(list name condition? true-expr false-expr)
     (datum->syntax stx `(cond [,condition? ,true-expr]
                               [else ,false-expr]))]))
(my-if-with-match #t
       ((lambda () (displayln "true") "true"))
       ((lambda () (displayln "false") "false")))

; begin-for-syntax ---------------------------------------------------

; In the previous, we used `for-syntax' to require the racket/match
; module because we need to use match at compile time. Now, what if we
; wanted to define our own helper function to be used by a macro? One
; way to do that is put it in another module, and then require it
; using `for-syntax', just like we did with the racket/match module.

; If instead we want to put the helper in the same module, we can't
; simply `define' it and use it -- the defintion would exist at run
; time, but we need it a compile time. The answer is to put the
; definition of the helper fucntion inside `begin-for-syntax'. Or
; `define-for-syntax' which composes `begin-for-syntax' and `define'.
