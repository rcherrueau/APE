#lang reader "../lang.rkt"

;; Prelude
(class X) (class Unit)

;; Figure 3: Owner Context Example
(class Link{n}
  (field [next : Θ/Link{n}])  ;; `next` is owned by the same owner as
                              ;; the instance it is enclosed in.
  (field [data : n/X])        ;; data belongs to the context `n`.

  (def (init [inData : n/X] -> Θ/Link{n})
    (set-field! this data inData)
    this))

(class XStack{m}
  (field [top : rep/Link{m}])

  (def (push [data : m/X] -> Unit)
    (let ([newTop : rep/Link{m}
                  (send (new rep/Link{m}) init data)])
      (set-field! newTop next top)
      (set-field! this top newTop)
      (new Unit)))

  (def (pop -> m/X)
    (let ([oldTop : rep/Link{m} top]
          [top    : rep/Link{m} (get-field oldTop next)])
      (get-field top data))))

(let ([world-stack : XStack{world} (new XStack{world})]
      [rep-stack : XStack{rep} (new XStack{rep})])
  (send world-stack push (new X))
  (send rep-stack push (new rep/X)))
