#lang reader "../lang.rkt"

;; Type def
(class FlatNet)
(class VM)
(class ListVMs)

;; Model OS services
(class Network{os}
  (field [flatNet : os/FlatNet]))

(class Compute{os}
  (def (bootVM [net : os/FlatNet] → VM) ???)
  (def (listVMs -> ListVMs) ???))

(class OS-SDK{a b}
  (field [network : a/Network])
  (field [compute : b/Compute])

  (def (init [net : a/Network]
             [cpt : b/Compute]
             -> OS-SDK{a b})
    (set-field! this network net)
    (set-field! this compute cpt)
    this)

  (def (bootVM -> VM)
    (send compute bootVM (get-field network flatNet)))

  (def (listVMs -> ListVMs)
    (send compute listVMs))
  )

;; openstack server create --network flat my-vm
(let ([network : Network{Site1} (new Network{Site1})]
      [compute : Compute{Site2} (new Compute{Site2})]
      [os-cli  : OS-SDK{Site1 Site2} (send (new OS-SDK{Site1 Site2})
                                           init network compute)])
  (send os-cli bootVM))
