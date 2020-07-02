#lang reader "../lang.rkt"

;; Prelude
(class Unit)

;; Figure 1: Car example
(class Engine
  (def (start -> Unit) ???)
  (def (stop  -> Unit) ???))

(class Driver #;... )

(class Car
  ;; `engine` is bound to the context of Car instances
  (field [engine : rep/Engine])
  ;; `driver` is bound to the world context
  (field [driver : world/Driver])

  (def (init -> Θ/Car)
    ;; New engine as part of the Car instance
    (set-field! this engine (new rep/Engine))
    this)

  (def (get-engine -> rep/Engine)
    engine)

  (def (set-engine! [e : rep/Engine] -> Unit)
    (set-field! this engine e)
    (new Unit))

  (def (go -> Unit)
    (send engine start)))


(let ([bob : world/Driver (new Driver)]  ;; No owner implicitly means world
      [car : Car    (send (new Car) init)])

  (set-field! car driver bob)
  (send car go)

  ;; ;; fails -- by giving `engine` the ownership type `rep`, then engine
  ;; ;; belongs to `car`. It is not accessible outside of this instance
  ;; ;; and therefore cannot be accessed by the root system as we try to
  ;; ;; do here.
  ;; (send (get-field car engine) stop)

  ;; ;; fails -- In contrast to Java, ownership type does not allow to
  ;; ;; access a private field from a method.
  ;; (send (send car get-engine) stop)

  ;; ;; fails -- The method `set-engine!` requires an argument that
  ;; ;; belongs to the `car` instance. However, the value of `e` belongs
  ;; ;; to the root system.
  ;; (let ([e : rep/Engine (new rep/Engine)])
  ;;   ;; fails, different rep
  ;;   (send car set-engine! e))

  )
