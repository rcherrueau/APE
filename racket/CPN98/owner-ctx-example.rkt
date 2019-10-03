#lang reader "surface-lang.rkt"
;; #lang s-exp "lang.rkt"

(class Link [n]
  (field [next : (Θ / (Link n))])
  (field [data : (n / X)])

  (def (init [inDate : (n / X)] → Unit)
    (set-field! this data inData)))

(class XStack [n]
  (field [top : (rep / (Link m))])

  (def (push [data : (m / X)] → Unit)
    (let ([top    : (rep / (Link m)) (get-field this top)]
          [newTop : (rep / (Link m)) (new (rep / (Link m)))]
          [_      : Unit             (send newTop init data)]
          [_      : Unit             (set-field! newTop next top)])
      (set-field! this top newTop)))

  (def (pop → (m / X))
    (let ([oldTop : (rep / (Link m)) (get-field this top)]
          [top    : (rep / (Link m)) (get-field oldTop top)])
      (get-field top data))))

(new XStack)
