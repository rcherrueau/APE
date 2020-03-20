#lang reader "../lang.rkt"

;; Type def
(class FlatNet) (class VM)

;; Model OS services
(class Network [os]
  (field [flatNet : os/FlatNet]))

(class Compute [os]
  (def (bootVM [net : os/FlatNet] → VM) ???))

;; openstack server create --network flat my-vm
(let ([network : world/[Network Site1] (new (Network Site1))]
      [compute : world/[Compute Site2] (new (Compute Site2))])
  (send compute bootVM
        (get-field network flatNet)))
