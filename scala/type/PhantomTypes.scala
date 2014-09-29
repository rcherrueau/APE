/** Phantom Types */

/** RacketModule
  *
  * [[http://james-iry.blogspot.fr/2010/10/phantom-types-in-haskell-and-scala.html]]
  */
object RocketModule {
  sealed trait NoFuel
  sealed trait Fueled
  sealed trait NoO2
  sealed trait HasO2

  // The Rocket data type takes two type parameters, fuel and o2. But
  // the constructor doesn't touch them. I don't export the
  // constructor so only this module can create rockets.
  final case class Rocket[Fuel, O2] private[RocketModule] ()

  // Produces a rocket with no fuel and no o2.
  def createRocket = Rocket[NoFuel, NoO2]()

  // Takes a rocket with no fuel and return one with fuel. It doesn't
  // touch the o2.
  def addFuel[O2](x: Rocket[NoFuel, O2]) = Rocket[Fueled, O2]()

  // Adds o2 without touching the fuel.
  def addO2[Fuel](x: Rocket[Fuel, NoO2]) = Rocket[Fuel, HasO2]()

  // Launch will only launch a rocket with both fuel and o2
  def launch(x: Rocket[Fueled, HasO2]) = "blastoff"

  // This is just a pretty way of stringing things together, stolen
  // shamelessly from F#. Adding infix operations is a bit verbose in
  // Scala.
  case class Pipeable[V](value: V) {
    def |>[R](f: V => R) = f(value)
  }
  implicit def toPiped[V](value: V) = Pipeable(value)


  def test1 = createRocket |> addFuel |> addO2 |> launch
  def test2 = createRocket |> addO2 |> addFuel |> launch
  // Won't type checks
  // def test3 = createRocket |> addO2 |> launch
  // def test4 = createRocket |> addFuel |> launch
  // def test5 = createRocket |> addFuel |> addO2 |> addFuel |> launch
}

/** Scala types of types
  *
  * [[http://ktoso.github.io/scala-types-of-types/#phantom-type]]
  */
// Use it to enforce some logic. We want a sevice class which has
// `start' and `stop' methods. We want to guarantee that you cannot
// (using the type system) start an already started sevice, and
// vice-versa.
object ServiceModule {
  // Making the `ServiceState' sealed assures that no-one can add
  // another state to our system.
  sealed trait ServiceState
  final class Started extends ServiceState
  final class Stopped extends ServiceState

  // Let's defina a class `Service' which take `State' type parameter.
  // We don't export the constructor so only this module can create
  // Service.
  class Service[State <: ServiceState] private () {
    def start[T >: State <: Stopped]() = this.asInstanceOf[Service[Started]]
    def stop[T >: State <: Started]() = this.asInstanceOf[Service[Stopped]]
  }

  object Service {
    def create() = new Service[Stopped]
  }
}

/**
  * [[https://gist.github.com/milessabin/c9f8befa932d98dcc7a4]]
  * [[https://groups.google.com/forum/#!msg/scala-language/4SQt-n1l9Zk/jIgzYrJiMNMJ]]
  */
object NotSubtype {
  // Encoding for "A is not a subtype of B"
  trait <:!<[A, B]

  // Uses ambiguity to rule out the cases we're trying to exclude
  implicit def nsub[A, B] : A <:!< B = null
  implicit def nsubAmbig1[A, B >: A] : A <:!< B = null
  implicit def nsubAmbig2[A, B >: A] : A <:!< B = null

  sealed abstract class Data
  case class Raw() extends Data
  case class Pull() extends Data
  case class Encrypted() extends Data

  // Type alias for context bound
  type ¬[T <: Data] = {
    type λ[α <: Data] = α <:!< T
  }


  def foo[R <: Data : ¬[Encrypted]#λ](t : R) = t

  foo(Raw())
  foo(Pull())
  // foo(Encrypted()) // Doesn't compile
}
