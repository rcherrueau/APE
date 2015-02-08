/** Functional Progarming in Scala, Chapter 8 */
package fpinscala

object ScalaCheck {
  import org.scalacheck.{Gen => SCGen}
  import org.scalacheck.Prop.{forAll => scForAll}
  import org.scalacheck.Prop.{throws => scThrows}

  // A generator of lists of integers between 0 and 100
  val intList = SCGen.listOf(SCGen.choose(0,100))

  // A property that specifies the behavior of the List.reverse
  // method. We use `&&` to combine two properties. The resulting
  // property will hold only if neither property can be falsified by
  // any of the generated test cases.
  val prop =
    scForAll (intList) {
      // Check that reversing a list twice gives back the original list.
      ns => ns.reverse.reverse == ns
    } && scForAll (intList) {
      // Check that the first element becomes the last element after
      // reversal.
      ns => ns.headOption == ns.reverse.lastOption
    }

  // A property which is obviously false.
  val failingProp = scForAll (intList) { ns => ns.reverse == ns }

  // scala> import fpinscala.ScalaCheck._
  // scala> prop.check
  // + OK, passed 100 tests.
  // scala> failingProp.check
  // ! Falsified after 3 passed tests.
  // > ARG_0: List("0", "1")
  // > ARG_0_ORIGINAL: List("25", "82")

  // A property that specifies the behaviour of the `List.sum`
  val sumProp =
    // There are four mathematical properties which involve addition.
    // See http://www.aaamath.com/pro.htm
    scForAll (intList) {
      // (1) Commutative property: when two numbers are added, the
      // sum is the same regardless of the order of the addends.
      ns => ns.reverse.sum == ns.sum
    } && scForAll (intList) {
      // (2) Associative property: When three or more numbers are
      // added, the sum is the same regardless of the grouping of the
      // addends.
      ns => {
        val (ns1, ns2) = ns.splitAt(ns.length / 2)
        ns1.sum + ns2.sum
      } == ns.sum
    } && scForAll (intList) {
      // (3) Additive identity property: The sum of any number and
      // zero is the original number.
      ns => (ns :+ 0).sum == ns.sum
    } && scForAll (intList) {
      // (4) Distributive property: The sum of two numbers times a
      // third number is equal to the sum of each addend times the
      // third number.
      ns => ns.sum * 4 == ns.foldRight(0)(4 * _ + _)
    }

  // scala> import fpinscala.ScalaCheck._
  // scala> sumProp.check
  // + OK, passed 100 tests.

  // A property that specifies the behaviour of the `List.max`
  val maxProp =
    scForAll (SCGen.listOfN(1, SCGen.choose(0,100))) {
      // The max of a list of size 1 is the head of the list
      ns => ns.max == ns.head
    } && scForAll (intList) {
      // The max is greater than or equal to all elements of the list.
      ns => ns.forall(ns.max >= _)
    } && scForAll (SCGen.nonEmptyListOf(SCGen.choose(0,100))) {
      // The max is an element of that list for non empty list
      ns => ns.exists(ns.max == _)
    } && scThrows (
      // The max of an empty list throws an exception
      classOf[UnsupportedOperationException])(List[Int]().max)

  // scala> import fpinscala.ScalaCheck._
  // scala> maxProp.check
  // + OK, passed 100 tests.
}

// Combining prop values using combinator like `&&` required check to
// return some meaningful value. What type should that value have?
// Well, let's consider at minimum, we need to know whether the
// property succeeded or failed:
trait Prop1 {
  /** Property runner. */
  def check: Boolean

  /** Property composer.
    *
    * Assuming the above representation of `check`, implementing `&&`
    * is simply running both `check` method and compose result using
    * boolean `&&` operator:
    */
  val me = this
  def &&(p: Prop1): Prop1 = new Prop1 {
    def check = me.check && p.check
  }

  // And more idiomatic way to access `this` in `&&`.
  def and(p: Prop1): Prop1 = new Prop1 {
    def check = Prop1.this.check && p.check
  }
}

// In the previous representation, `Prop1` is nothing more than a
// non-strict Boolean, and usual boolean function (&&, ||, ...) can be
// defined. But boolean is insufficient if a property fails. We might
// want to know:
// - how many tests succeeded first, and what arguments produced the
//   failure.
// - If a property succeeds, how many tests it ran.
trait Prop2 {
  type SuccessCount = Int // Help in the readability of the API
  type FailedCase = String
  type Result = Either[(FailedCase, SuccessCount), SuccessCount]

  def check: Result

  def &&(p: Prop2): Prop2 = new Prop2 {
    def check = Prop2.this.check match {
      case Right(x) => p.check match {
        case Right(y) => Right(x+y)
        case Left((s,y)) => Left(s, x+y)
      }
      case l@Left(_) => l
    }
  }
}

// However, our previous implementation of Prop misses one important
// information: the number of tests to run. Let's implement this in
// the `run` method and returns the result in a new data type `Result`
// that shows or intent more cleary than with the Either type.
import Prop._
sealed trait Result {
  def isFalsified: Boolean
}
/** Indicates that all tests passed. */
case object Passed extends Result {
  def isFalsified = false
}
/** Indicates that one of the test cases falsified the property. */
case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  def isFalsified = true
}

// To generate values from passed generators, will also need a `RNG`
// seed. Thus, `run` method also takes a `RNG`.
case class Prop(run: (TestCases, RNG) => Result) {
  def check: Result = run(100, Simple(42))

  def &&(p: Prop): Prop = Prop(
    (n, rng) => run(n, rng) match {
      case Passed => p.run(n, rng)
      case x => x
    })

  def ||(p: Prop): Prop = Prop(
    (n, rng) => run(n, rng) match {
      // In case of failure, run the other prop.
      case Falsified(m, _) => p.run(n, rng)
      case x => x
    }
  )
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int

  /** Property creator. */
  def forAll[A](ga: Gen[A])(f: A => Boolean): Prop = Prop(
    (n, rng) => (streamGenVal(ga)(rng).zipWithIndex).take(n).map {
      // TODO: handle the case when `f(a)` throws an exception
      case (a, i) => if (f(a)) Passed
                     else Falsified(a.toString, i)
    }.find(_.isFalsified) getOrElse(Passed)
  )

  private def streamGenVal[A](ga: Gen[A])(rng: RNG): Stream[A] = {
    def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) map {
        case(a, s) => a #:: unfold(s)(f)
      } getOrElse Stream.Empty

    unfold(rng) { r => Some(ga.sample.run(r)) }
  }
}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample flatMap { a => f(a).sample })

  def map[B](f: A => B): Gen[B] =
    Gen(sample map { a => f(a) })

  /** Generates list of arbitrary length of this. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { n => Gen.listOfN(n, this) }
}

object Gen {
  /** Generates integers in the range `start` to `stopExclusive`. */
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG2.nonNegativeInt map { n =>
          start + n  % (stopExclusive - start) })

  def choose2(start: Int, stopExclusive: Int): (Gen[Int], Gen[Int]) =
    (choose(start, stopExclusive), choose(start, stopExclusive))


  /** Always generates the value a */
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(RNG2.nonNegativeInt map { n => if (n % 2 > 0) true
                                       else false })

  /** Generates lists of length `n` usging the generator `g`. */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  /** Makes a generator result optional. */
  def option[A](ga: Gen[A]): Gen[Option[A]] =
    ga flatMap { a => boolean map { if (_) Some(a) else None }}

  /** Combines two generators of the same type into one.
    *
    * Union pulls values from each generator with equal likelihood.
    */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap { if (_) g1 else g2 }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Proba = g1._2 / (g1._2 + g2._2)
    val g2Proba = 1.0 - g1Proba

    Gen(RNG2.double flatMap (d => if (g1._2 < g2._2)  {
                               if (d <= g1Proba) g1._1.sample
                               else g2._1.sample
                             } else {
                               if (d <= g2Proba) g2._1.sample
                               else g1._1.sample
                             }))
  }

  def sequence[A](gs: List[Gen[A]]): Gen[List[A]] =
    gs.foldRight[Gen[List[A]]](Gen(State.unit(Nil)))((hd, rest) =>
      hd flatMap { a => rest map {a :: _}})

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
}

object FPInScalaPropTest extends App {
  val seed = Simple(42)

  // Gen.choose
  (for {
    a <- Gen.choose(1,10).sample
    b <- Gen.choose(1,10).sample
    c <- Gen.choose(1,10).sample
    d <- Gen.choose(1,10).sample
    e <- Gen.choose(1,10).sample
    f <- Gen.choose(1,10).sample
    g <- Gen.choose(1,10).sample
    h <- Gen.choose(1,10).sample
    i <- Gen.choose(1,10).sample
    j <- Gen.choose(1,10).sample
   } yield println(List(a,b,c,d,e,f,g,h,i,j))) run(seed)

  // Gen.boolean
  (for {
    a <- Gen.boolean.sample
    b <- Gen.boolean.sample
    c <- Gen.boolean.sample
    d <- Gen.boolean.sample
    e <- Gen.boolean.sample
    f <- Gen.boolean.sample
    g <- Gen.boolean.sample
    h <- Gen.boolean.sample
    i <- Gen.boolean.sample
    j <- Gen.boolean.sample
   } yield println(List(a,b,c,d,e,f,g,h,i,j))) run(seed)

  // Gen.listOfN
  Gen.listOfN(10, Gen.choose(1, 10)).sample map (println) run(seed)

  // Gen flatMap / map
  (for {
    a <- Gen.choose(1,10)
    b <- Gen.choose(1,10)
    c <- Gen.choose(1,10)
    d <- Gen.choose(1,10)
    e <- Gen.choose(1,10)
    f <- Gen.choose(1,10)
    g <- Gen.choose(1,10)
    h <- Gen.choose(1,10)
    i <- Gen.choose(1,10)
    j <- Gen.choose(1,10)
   } yield println(List(a,b,c,d,e,f,g,h,i,j))).sample run(seed)

  (for {
    a <- Gen.boolean
    b <- Gen.boolean
    c <- Gen.boolean
    d <- Gen.boolean
    e <- Gen.boolean
    f <- Gen.boolean
    g <- Gen.boolean
    h <- Gen.boolean
    i <- Gen.boolean
    j <- Gen.boolean
   } yield println(List(a,b,c,d,e,f,g,h,i,j))).sample.run(seed)

  // Gen->listOf
  (Gen.choose(1,10).listOfN(Gen.choose(10,11)) map (println)).
    sample run(seed)

  // Gen.weighted
  (for {
     g1g2s <- Gen.listOfN(10, (Gen.weighted(
                                 (Gen.choose(1,10)  -> .1),
                                 (Gen.choose(11,20) -> .9))))
   } yield println(g1g2s)).sample run(seed)
}
