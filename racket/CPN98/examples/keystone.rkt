#lang reader "../lang.rkt"

(class Unit #;... )
(class Token #;... )
(class Bool #;... )

(class Keystone
  (def (get-token → owner/Token)
    ???)

  (def (validate-token [token : owner/Token] → world/Bool)
    ???)
  )

(class Main (p q)
  (def (my-main [k1 : p/Keystone]
                [k2 : q/Keystone] → Unit)
    (let ([t : p/Token (send k1 get-token)])
      (send k1 validate-token t)  ; p/Token => Well-type
      (send k2 validate-token t)  ; q/Token => Ill-typed if p != q
      )))

(let ([k1 : os1/Keystone (new os1/Keystone)]
      [k2 : os2/Keystone (new os2/Keystone)]
      [main : Main (new Main)])
  ;; At the end, well-typed in `my-main`. `p` and `q` are
  ;; equals.
  (send main my-main k1 k1)
  ;; Ill-typed p and q are different
  (send main my-main k1 k2)
  )
