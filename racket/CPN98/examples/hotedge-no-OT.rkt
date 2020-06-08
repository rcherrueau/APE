#lang reader "../lang.rkt"

;; Type def
(class FlatNet)
(class VM)
(class ListVMs)

;; Model OS services
(class Network
  (field [flatNet : FlatNet]))

(class Compute
  (def (bootVM [net : FlatNet] → VM) ???)
  (def (listVMs -> ListVMs) ???))

(class OS-SDK
  (field [network : Network])
  (field [compute : Compute])

  (def (init -> OS-SDK)
    (set-field! this network (new Network))
    (set-field! this compute (new Compute))
    this)

  (def (bootVM -> VM)
    (send compute bootVM (get-field network flatNet)))

  (def (listVMs -> ListVMs)
    (send compute listVMs))
  )

;; openstack server create --network flat my-vm
(let ([network : Network (new Network)]
      [compute : Compute (new Compute)]
      [os-cli  : OS-SDK  (send (new OS-SDK) init)])
  (send os-cli bootVM)
  (send os-cli listVMs))
