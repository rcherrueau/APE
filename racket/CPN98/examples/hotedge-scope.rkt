#lang reader "../lang.rkt"

;; Type def
(class FlatNet)
(class VM)
(class ListVMs)

;; Model OS services
(class Network
  (field [flatNet : app/FlatNet]))

(class Compute
  (def (bootVM [net : app/FlatNet] -> VM) ???)
  (def (listVMs -> ListVMs) ???))

(class OS-SDK
  (field [network : app/Network])
  (field [compute : app/Compute])

  (def (init → OS-SDK)
    (set-field! this network (new app/Network))
    (set-field! this compute (new app/Compute))
    this)

  (def (bootVM → VM)
    (send compute bootVM (get-field network flatNet)))

  (def (listVMs -> ListVMs)
    (send compute listVMs))
  )

(let ([os-cli : OS-SDK (send (new OS-SDK) init)])
  ;; Boot VM with Compute and Network at App location
  ;; > well-typed
  (send os-cli bootVM)

  ;; Boot VM with Compute at Site1 and Network at Site2
  ;; > ill-typed
  (scope ([app/Network Site1/Network]
          [app/Compute Site2/Compute])
    (send os-cli bootVM))

  ;; Boot VM with Compute at Site1 and Network at Site1
  ;; > well-typed
  (scope ([app/Network Site1/Network]
          [app/Compute Site1/Compute])
    (send os-cli bootVM))

  (scope ([app/Compute (& Site1/Compute Site2/Compute)])
    (send os-cli listVMs))

  ;; It is now clear for me that the object oriented programming is
  ;; not the best tool for this
  )
