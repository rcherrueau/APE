#lang reader "../lang.rkt"

(class Unit #;... )
(class Token #;... )
(class Boolean #;... )

(class Keystone
  (def (get-token → Θ/Token)
    ???)

  (def (validate-token [token : Θ/Token] → world/Boolean)
    ???)
  )

(class Main{p q}
  (def (my-main [k1 : p/Keystone]
                [k2 : q/Keystone] → Boolean)
    (let ([t : p/Token (send k1 get-token)])
      (send k1 validate-token t)  ; p/Token => Well-type
      (send k2 validate-token t)  ; q/Token => Ill-typed if p != q
      )))

(let ([k1 : os1/Keystone (new os1/Keystone)]
      [k2 : os2/Keystone (new os2/Keystone)]
      [main : Main (new Main{os1 os2})])
  ;; At the end, well-typed in `my-main`. `p` and `q` are
  ;; equals.
  (send main my-main k1 k1)
  ;; Ill-typed p and q are different
  (send main my-main k1 k2)
  )
