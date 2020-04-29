#lang reader "../lang.rkt"

(class Unit #;... )

(class Engine
  (def (start → Unit) ???)
  (def (stop  → Unit) ???))

(class Driver #;... )

(class Car
  (field [engine : rep/Engine])
  (field [driver : world/Driver])

  (def (init → Unit)
    (set-field! this engine (new rep/Engine))
    ???)

  (def (get-engine → rep/Engine)
    engine)

  (def (set-engine! [e : rep/Engine] → rep/Engine)
    (set-field! this engine e))

  (def (go → Unit)
    (send engine start)))


(let ([bob : world/Driver (new Driver)]
      [car : wordl/Car    (new Car)])
  (send car init)
  (set-field! car driver bob)
  (send car go)
  (send (get-field car engine) stop) ;; fails
  (send (send car get-engine) stop)  ;; fails but not in Java
  (let ([e : rep/Engine (new rep/Engine)])
    ;; fails, different rep
    (send car set-engine! e))
  )
