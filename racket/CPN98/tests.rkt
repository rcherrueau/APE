#lang s-exp "lang.rkt"

(class Foo0 [bar])

(class Foo1 [bar]
  (field [f1 : Integer])
  (field [f2 : String]))

(class Foo2 [bar]
  (def (def0) foo)
  (def (def1 [arg1 : Integer]) foo)
  (def (def2 [arg1 : Integer][arg2 : Integer]) foo))

(class Foo3 [bar]
  (field [f1 : Integer])
  (field [f2 : String])
  (def (def0) foo)
  (def (def1 [arg1 : Integer]) foo)
  (def (def2 [arg1 : Integer][arg2 : Integer]) foo))

;; 1
;; (new Foo0)
;; (send foo0 def0)
;; (send foo0 def1 bar)
;; (send foo0 def2 bar1 bar2)
;; (send foo0 def2 bar1 bar2 1)
;; (let (foo : Interger 1) 1)
;; (let (foo : Interger 1)
;;   (let (foo0 : Foo0 (new Foo0))
;;     (send foo0 def1 foo)))
;; (set-field! foo0 f1 1)
(get-field foo0 f1)
