/** Functional Programming in Scala, Chapter 6 */
package fpinscala

trait RNG { def nextInt: (Int, RNG) }
case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = Simple(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt

    ((i1, i2) -> rng3)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt

    (if (i == Int.MinValue) Int.MaxValue
     else Math.abs(i),
     rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)

    (i / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng2)

    ((i, d) -> rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = int(rng2)

    ((d, i) -> rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)

    ((d1, d2, d3) -> rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (i, rng2) = rng.nextInt
      val (l, rng3) = ints(count - 1)(rng2)
      (i :: l, rng3)
    } else (Nil, rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  // Passes the RNG state through without using it.
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // Transforms the *output* of a state action without modifying the
  // state itself.
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    (rng: RNG) => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // Using map
  def double_2: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A])(rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  // Using map2
  def both[A,B](ra: Rand[A])(rb: Rand[B]): Rand[(A, B)] =
    map2(ra)(rb)((_, _))

  def intDouble_2: Rand[(Int, Double)] =
    both(int)(double)

  def doubleInt_2: Rand[(Double, Int)] =
    both(double)(int)

  def randIntDouble: Rand[(Int, Double)] =
    both(int)(double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double)(int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil:List[A]))(
      (h, rest) => map2(h)(rest)(_ :: _))

  def ints_2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    rng => {
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n-1) - mod >= 0) (mod, rng2)
      else nonNegativeLessThan(n)(rng)
    }

  def flatMap[A,B,C](s: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      f(a)(rng2)
    }

  // Using flatMap
  def nonNegativeLessThan_2(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan_2(n)
    }

  def map_2[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2_2[A,B,C](ra: Rand[A])(rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))

}

// // More general signature for working with state actions.
// object State {
//   // `S` stands for State type
//   def map[S,A,B](a: S => (A, S))(f: A => B): S => (B, S) =
//     state => {
//       val (v, state2) = a(state)
//       (f(v), state2)
//     }
//
//   // More general type than `Rand`, for handling any type of state.
//   type State[S, +A] = S => (A, S)
// }
//
// // We might want to write it as its own class, wrapping the underlying
// // function like this:

// More general type than `Rand`, for handling any type of state.
// `S` stands for State type.
case class State[S,+A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S,B]): State[S,B] = State(
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

  def map[B](f: A => B): State[S,B] =
    this flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] =
    this flatMap(a => sb flatMap( b => State.unit(f(a,b))))
}

object State {
  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  def sequence[S,A](sas: List[State[S,A]]): State[S,List[A]] =
    sas.foldRight(unit[S,List[A]](Nil))(
      (sa, rest) => sa.map2(rest)(_ :: _))

  // *State combinators*

  // Combinator that modifies the state.
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get       // Gets the current state and assigns it to `s`
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`
  } yield ()

  // Combinator that gets the current state.
  def get[S]: State[S,S] = State((s:S) => (s, s))

  // Combinator that sets the current state.
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

// Same as RNG but using State monad.
object RNG2 {
  type Rand[+A] = State[RNG,A]

  val int: Rand[Int] = State(_.nextInt)
  val ns: Rand[List[Int]] = for {
     x <- nonNegativeLessThan(5)
     y <- int
    xs <- ints(x)
  } yield xs.map(_ % y) // xs here is the unboxed Rand[List[Int]] xs.
                        // Thus, map is map on the List.

  def randomPair: Rand[(Int, Int)] = State(
    s => {
      val (i1, s1) = int.run(s)
      val (i2, s2) = int.run(s1)
      ((i1,i2) -> s2)
    })

  def nonNegativeInt: Rand[Int] = State(
    s => {
      val (i, s1) = int.run(s)
      (if (i == Int.MinValue) Int.MaxValue
       else Math.abs(i),
       s1)
    })

  def double: Rand[Double] =
    nonNegativeInt map { _ / (Int.MaxValue.toDouble + 1) }

  def intDouble: Rand[(Int, Double)] =
    int.map2(double)((_,_))

  def doubleInt: Rand[(Double, Int)] =
    double.map2(int)((_,_))

  def double3: Rand[(Double, Double, Double)] =
    double flatMap { d1 =>
      double flatMap { d2 =>
        double flatMap {d3 => State.unit((d1, d2, d3)) }}}

  def nonNegativeEven: Rand[Int] =
    nonNegativeInt map { i => i - i % 2 }

  def both[A,B](ra: Rand[A])(rb: Rand[B]): Rand[(A, B)] =
    ra.map2(rb)((_, _))

  def ints(count: Int): Rand[List[Int]] =
    State.sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    nonNegativeInt flatMap {i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) State.unit(mod)
      else nonNegativeLessThan(n)
    }
}

object FPInScalaStateTest extends App {
  val rng = Simple(42)

  // Purely functional random number generation
  println("Purely functional random number generation")
  val (i, rng2) = RNG.randomPair(rng); println(i)
  val (i2, rng3) = RNG.nonNegativeInt(rng2); println(i2)
  val (d, rng4) = RNG.double(rng3); println(d)
  RNG.map(RNG.double_2)(println)(rng3)
  val (p1, rng5) = RNG.intDouble(rng4); println(p1)
  RNG.map(RNG.intDouble_2)(println)(rng4)
  RNG.map(RNG.randIntDouble)(println)(rng4)
  RNG.map_2(RNG.map2_2(RNG.int)(RNG.double)((_,_)))(println)(rng4)
  val (p2, rng6) = RNG.doubleInt(rng5); println(p2)
  RNG.map(RNG.doubleInt_2)(println)(rng5)
  RNG.map(RNG.randDoubleInt)(println)(rng5)
  RNG.map_2(RNG.map2_2(RNG.double)(RNG.int)((_,_)))(println)(rng5)
  val (t3, rng7) = RNG.double3(rng6); println(t3)
  val (l, rng8) = RNG.ints(5)(rng7); println(l)
  RNG.map(RNG.ints(5))(println)(rng7)
  val (_, rng9) = RNG.map(RNG.nonNegativeEven)(println)(rng8)
  val (_, rng10) =
    RNG.map(RNG.sequence(List.fill(10)(RNG.nonNegativeLessThan(10))))(println)(rng9)
  RNG.map(RNG.sequence(List.fill(10)(RNG.nonNegativeLessThan_2(10))))(println)(rng9)
  RNG.map_2(RNG.sequence(List.fill(10)(RNG.nonNegativeLessThan_2(10))))(println)(rng9)

  def rollDie: RNG.Rand[Int] =
    RNG.map(RNG.nonNegativeLessThan(6))(_ + 1)

  println(RNG.sequence(List.fill(20)(rollDie))(rng)._1)

  // General state action data type
  print("\nGeneral state action data type")
  (for {
     i1 <- RNG2.randomPair
     i2 <- RNG2.nonNegativeInt
      d <- RNG2.double
     p1 <- RNG2.intDouble
     p2 <- RNG2.doubleInt
     t3 <- RNG2.double3
      l <- RNG2.ints(5)
    nne <- RNG2.nonNegativeEven
    nnl <- State.sequence(List.fill(10)(RNG2.nonNegativeLessThan(10)))
     rl <- State.sequence(List.fill(20)(rollDie2))
   } yield println(List(i1, i2, d, p1, p2, t3, l, nne, nnl, rl).
                     foldRight("")("\n" + _  + _))).run(rng)

  def rollDie2: RNG2.Rand[Int] =
    RNG2.nonNegativeLessThan(6) map { _ + 1 }

  println(RNG2.ns.run(rng)._1)

  // *Candy Dispenser*
  //
  // Machine has two types of input:
  // 1) Insert a coin
  // 2) Turn the knob (to dispense candy)
  //
  // Machine can be in two states:
  // 1) Locked
  // 2) Unlocked
  //
  // Machine also tracks how many candies are left and how many coins
  // it contains.

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  // Machine rules:
  // - Inserting a coin into a locked machine will cause it to unlock
  // if there's any candy left.
  // - Turning the knob on unlocked machine will cause it to dispense
  // candy and become locked.
  // - Turning the knob on locked machine or inserting a coin into an
  // unlocked machine does nothing.
  // - A machine that's out of candy ignores all inputs.
  def simulateMachine(inputs: List[Input]): State[Machine,(Int, Int)] = {
    // For each input, computes the expected state
    val is: List[State[Machine, (Int,Int)]] = inputs.map(i =>
      State(
        (m: Machine) => m match {
          case Machine(_,0,_) => ((m.coins, m.candies) -> m)
          // Locked
          case Machine(true, ca, co) => i match {
            case Coin => ((co+1, ca) -> Machine(false, ca, co+1))
            case Turn => ((m.coins, m.candies) -> m)
          }
          // Unlocked
          case Machine(false, ca, co) => i match {
            case Coin => ((m.coins, m.candies) -> m)
            case Turn => ((co, ca-1) -> Machine(true, ca-1, co))
          }
        }))

    // Does the sequence of all state.
    val ms: State[Machine,List[(Int, Int)]] = State.sequence(is)

    // Returns the last state.
    ms map { _.last }
  }
  // Better version with `modify` that modifies the current state into
  // a new state. Instead of keeping the whole state, we focus on the
  // last one.
  def simulateMachine_2(inputs: List[Input]): State[Machine,(Int, Int)] = {
    // We modify the state until the last one
    val lastState: State[Machine, List[Unit]] = State.sequence(
      inputs.map(i =>
        State.modify(
          (m: Machine) => m match {
            case Machine(_,0,_) => m
            // Locked
            case Machine(true, ca, co) => i match {
              case Coin => Machine(false, ca, co+1)
              case Turn => m
            }
            // Unlocked
            case Machine(false, ca, co) => i match {
              case Coin => m
              case Turn => Machine(true, ca-1, co)
            }
          })))

    // We get the state and we return the state with coins and candies
    // values.
    lastState flatMap { (_: List[Unit]) =>
      // Get the state
      State.get map { m => (m.coins, m.candies) } }
  }
  // Same as simulateMachine_2 but with for-comprehension
  def simulateMachine_3(inputs: List[Input]): State[Machine,(Int, Int)] =
    // Here the return type stands for (m: Machine) => ((Int, Int),
    // Machine)
    for {
      _ <- State.sequence(inputs.map(
                            i => State.modify(
                              (s: Machine) => (i, s) match {
                                case (_, Machine(_, 0, _)) => s
                                case (Coin, Machine(false, _, _)) => s
                                case (Turn, Machine(true, _, _)) => s
                                case (Coin, Machine(true, ca, co)) =>
                                  Machine(false, ca, co+1)
                                case (Turn, Machine(false, ca, co)) =>
                                  Machine(true, ca-1, co)
                              })))
      s <- State.get
    } yield (s.coins, s.candies)

  println(simulateMachine(
            List(Coin, Turn, Coin, Turn, Turn, Coin, Coin, Turn)).run(
            Machine(true, 5, 10))._1)

  println(simulateMachine_2(
            List(Coin, Turn, Coin, Turn, Turn, Coin, Coin, Turn)).run(
            Machine(true, 5, 10))._1)

  println(simulateMachine_3(
            List(Coin, Turn, Coin, Turn, Turn, Coin, Coin, Turn)).run(
            Machine(true, 5, 10))._1)
}
