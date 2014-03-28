#lang racket

; Fear of Macros
; http://www.greghendershott.com/fear-of-macros/


; Most useful syntax transformers work by taking some input syntax,
; and rearranging the pieces into something else. As we saw, it's more
; convenient and less error-prone to use `match' to do pattern
; matching. It tunrs out that pattern-matching was one of the first
; improvements to be added to the Racket macro system. It's called
; `syntax-case' and has a shorthand for simple situations called
; `define-syntax-rule'.
; The way we specify the new syntax is similar. We d'ont need to do
; quasi-quoting and unquoting. We don't need to use `datum->syntax'.
; Instead, we supply a "template", which uses vaiable from the
; pattern.
(define-syntax (my-if-using-syntax-case stx)
  (syntax-case stx ()
    [(_ condition? right-expr left-expr)
     #'(cond [condition? right-expr]
             [else left-expr])]))
(my-if-using-syntax-case #t
       ((lambda () (displayln "true") "true"))
       ((lambda () (displayln "false") "false")))

; Pattern wariable vs. template -- fight! ----------------------------

; Let's say we want to define a function with a hyphenated name, a-b,
; but we supply the a and b separately:

;; (define-syntax (hyphen-define stx)
;;   (syntax-case stx ()
;;     [(_ a b (args ...) body ...)
;;      (let ([name (string->symbol (format "~a-~a" a b))])
;;        #'(define (name args ...) body ...))]))

; patter-matching.rkt:35:49: a: pattern variable cannot be used
; outside of a template in: a. We have no idea what this error message
; means. Well, let's try to work it out. The "template the error
; message refers to is the #'(define (name args ...) body ...) part.
; The `let' isn't part of that template. It soundslike we can't use
; `a' (or a `b') in the `let' part. But you can have as many template
; as you want. Thus, you can use `syntax' on a pattern variable. This
; makes another template, albeit a small, template.

(define-syntax (hyphen-define/wrong stx)
  (syntax-case stx ()
    [(_ a b (args ...) body ...)
     (let ([name (string->symbol (format "~a-~a" #'a #'b))])
       #'(define (name args ...) body ...))]))
(hyphen-define/wrong foo bar () #t)
;; (foo-bar)
;; foo-bar: undefined;
;;  cannot reference an identifier before its definition

; Be careful, our macro is defining a function with some name other
; than `foo-bar'. Using the Macro Stepper in DrRacket, it appears that
; the use of our macro `(hyphen-define/wrong1.1 foo bar () #t)'
; exanpded to `(define (name) #t)'. Instead, we wanted to expand to
; `(define (foo-bar) #t)'. Our template is using the symbol `name' but
; we wanted its value, such as `foo-bar' in this use of macro.

; Our pattern doesn't include `name' because we don't expect it in the
; original syntax -- indeed the whole point of this macro is to create
; it. So name can't be in the main pattern. Fine -- let's make an
; additional pattern. We can do that using an additional nested
; `syntax-case'.

(define-syntax (hyphen-define/ok1 stx)
  ; Normally our transformer function is given syntax by Racket, and
  ; we pass that syntax to `syntax-case'. But we can also create some
  ; syntax of our own, on the fly, and pass that to `syntax-case'.
  (syntax-case stx ()
    [(_ a b (args ...) body ...)
     ; The whole `(datum->syntax ...)' expression is syntax that we're
     ; creating on the fly. Then, we can give that to `syntax-case',
     ; and match it using a pattern variable named `name'. Voilà, we
     ; have a new pattern variable. We can use it in a template, and
     ; its value will go in the template.
     (syntax-case (datum->syntax stx
                                 (string->symbol (format "~a-~a"
                                                         (syntax->datum #'a)
                                                         (syntax->datum #'b))))
                  ()
       [name
        #'(define (name args ...) body ...)])]))

; with-syntax --------------------------------------------------------

; Instead of an additional, nested `syntax-case', we could use
; `with-syntax'. This rearranges the `syntax-case' to look more like a
; `let' statement. Also, it's more convenient if we need to define
; more than one patter variable.
(define-syntax (hyphen-define/ok2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body ...)
     ; Use the `with-syntax' as a let
     (with-syntax
         ([name (datum->syntax stx
                               (string->symbol (format "~a-~a"
                                                       (syntax->datum #'a)
                                                       (syntax->datum #'b))))])
       #'(define (name args ...) body ...))]))
(hyphen-define/ok2 foo bar () #t)
(foo-bar)

; Whether we use an additional `syntax-case' or use `with-syntax',
; either way we are simply defining additional pattern variable. Don't
; let the terminology and structure make it seem mysterious.

; We know that `let' doesn't let us use a binding in a subsequent one.
; Instead we can use nested lets. Or use a shortand for nesting,
; `let*'. Similary, instead of writing nested `with-syntax', we can
; use `with-syntax*'. One gotcha is that `with-syntax*' isn't provided
; by racket/base. We must require racket/syntax. Otherwise we may get
; a rather bewildering error message: ...: ellipses not allowed as an
; expression in: ...
(require (for-syntax racket/syntax))
(define-syntax (foo stx)
  (syntax-case stx ()
    [(_ a)
     (with-syntax* ([b #'a]
                    [c #'b])
       #'c)]))
(foo (displayln "foo"))

; format-id ----------------------------------------------------------

; There is a utility function in racket/syntax called `format-id' that
; lets us format identifier names more succintly than what we did
; above:
(require (for-syntax racket/syntax))
(define-syntax (hyphen-define/ok3 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body ...)
     (with-syntax
         ([name (format-id stx "~a-~a" #'a #'b)])
       #'(define (name args ...)
           body ...))]))
(hyphen-define/ok3 bar baz () #t)
(bar-baz)
