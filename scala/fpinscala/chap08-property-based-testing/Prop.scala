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
      ns => ns.contains(ns.max)
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
sealed trait Result {
  def isFalsified: Boolean
}
/** Indicates that all tests passed. */
case object Passed extends Result {
  def isFalsified = false
}
/** Indicates that one of the test cases falsified the property. */
case class Falsified(failure: String /* FailedCase */,
                     successes: Int /* SuccessCount */) extends Result {
  def isFalsified = true
}

// To generate values from passed generators, will also need a `RNG`
// seed. Thus, `run` method also takes a `RNG`.
case class Prop3(run: (Prop3.TestCases, RNG) => Result) {
  def check: Result = run(100, Simple(42))

  def &&(p: Prop3): Prop3 = Prop3(
    (n, rng) => run(n, rng) match {
      case Passed => p.run(n, rng)
      case x => x
    })

  def ||(p: Prop3): Prop3 = Prop3(
    (n, rng) => run(n, rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(n, rng)
      case x => x
    }
  )

  // In case of failure of the two branches print message of the two.
  def tag(msg: String): Prop3 = Prop3(
    (n, rng) => run(n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  )
}

object Prop3 {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int

  /** Property creator. */
  def forAll[A](ga: Gen[A])(f: A => Boolean): Prop3 = Prop3(
    (n, rng) =>
    (streamGenVal(ga)(rng).zipWith(Stream.from(1)))((_,_)).take(n).map {
      // TODO: handle the case when `f(a)` throws an exception
      case (a, i) => if (f(a)) Passed
                     else Falsified(a.toString, i)
    }.find(_.isFalsified) getOrElse(Passed)
  )

  private def streamGenVal[A](ga: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng) { r => Some(ga.sample.run(r)) }
  }
}

// Prop3 is OK but to implement test case minimization we wanna use
// sized generation strategy. We simply generate our test case in
// order of increasing size and complexity. Thus, we put `Prop` in
// charge of invoking the underlying generator with various size and
// we parameter `Prop` with a *maximum size*.
case class Prop(run: (Prop.MaxSize, Prop.TestCases, RNG) => Result) {
  def check: Result = ???

  def &&(p: Prop): Prop = Prop(
    (m, n, rng) => run(m, n, rng) match {
      case Passed => p.run(m, n, rng)
      case x => x
    })

  def ||(p: Prop): Prop = Prop(
    (m, n, rng) => run(m, n, rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(m, n, rng)
      case x => x
    }
  )

  // In case of failure of the two branches print message of the two.
  def tag(msg: String): Prop = Prop(
    (m, n, rng) => run(m, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n> " + e, c)
      case x => x
    }
  )
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  def run(p: Prop,
          maxSize: MaxSize = 100,
          testCases: TestCases = 100,
          rng: RNG = Simple(42)): Unit =
    println(
      p.run(maxSize, testCases, rng) match {
        case Passed => s"+ OK, passed ${testCases} test."
        case Falsified(msg, n) =>
          s"! Falsified after ${n} test.\n"+
          s"> ${msg}"
      })

  def forAll[A](sg: SGen[A])(f: A => Boolean): Prop =
    // forAll(g.forSize)(f)
    forAll(sg(_))(f) // Using `SGen.apply`. It Gives the feeling of
                     // using SGen as a function.

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop(
    (max, n, rng) => {
      // Make one property per size but no more than `n` property
      // because `forAll` will be test `n` times:
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map { i => forAll(g(i))(f) }
      // For each size, we have to generate `casesPerSize` test case:
      val casesPerSize = (n + max - 1) / max
      // Combines all property into one:
      val prop: Prop =
        props.map {
          p => Prop((max, _, rng) => p.run(max, casesPerSize, rng))
        }.toList.reduce(_ && _)

      prop.run(max, n, rng)
    })

  def forAll[A](g: Gen[A])(f: A => Boolean): Prop = Prop(
    (m, n, rng) =>
    (streamGenVal(g)(rng).zipWith(Stream.from(1)))((_,_)).take(n).map {
      case (a,i) => try {
        if (f(a)) Passed
        else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified) getOrElse(Passed))

  private def streamGenVal[A](ga: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng) { r => Some(ga.sample.run(r)) }
  }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"> generated an exception: ${e.getMessage}\n"
}

case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample flatMap { a => f(a).sample })

  def map[B](f: A => B): Gen[B] =
    Gen(sample map { a => f(a) })

  /** Generates list of arbitrary length of this. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { n => Gen.listOfN(n, this) }

  /** Converts Gen to SGen */
  def unsized: SGen[A] = SGen(n => this)
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

  /** Generates lists of size `n` usging the generator `g`. */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  /** Generates lists of arbitrary size (between 0 and 100) with `g` */
  def listOf_v1[A](g: Gen[A]): Gen[List[A]] =
    g.listOfN(Gen.choose(0,100))

  /** Generates lists of a requested size. */
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, g))

  /** Generates non empty list lists of a requested size. */
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN((n max 1), g))

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
}

// Use size generation as strategy for test case minimization. A size
// generator is just a function that takes a size and produce a
// generator.
case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n) // Enables the call of SGen
                                         // like a function.

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(n => forSize(n) flatMap (f))

  def map[B](f: A => B): SGen[B] =
    SGen(n => forSize(n) map (f))
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
  (Gen.choose(1,10).listOfN(Gen.unit(10)) map (println)).
    sample run(seed)

  // Gen.weighted
  (for {
     g1g2s <- Gen.listOfN(10, (Gen.weighted(
                                 (Gen.choose(1,10)  -> .1),
                                 (Gen.choose(11,20) -> .9))))
   } yield println(g1g2s)).sample run(seed)

  // Prop3 tests
  val intList = Gen.listOf_v1(Gen.choose(0,100))

  // Valid prop
  println(
    (Prop3.forAll (intList) {
       ns => ns.reverse.reverse == ns
     } && Prop3.forAll (intList) {
       ns => ns.headOption == ns.reverse.lastOption
     }).check)

  // Falsified prop
  println(
    (Prop3.forAll (intList) {
       ns => ns.reverse.reverse == ns
     } && Prop3.forAll (intList) {
       ns => ns.reverse == ns
     }).check)

  // Valid prop
  println(
    (Prop3.forAll (intList) {
       ns => ns.reverse.reverse == ns
     } || Prop3.forAll (intList) {
       ns => ns.reverse == ns
     }).check)

  // Falsified prop
  println(
    (Prop3.forAll (intList) {
       ns => ns.reverse == ns
     } || Prop3.forAll (intList) {
       ns => ns.reverse == ns
     }).check)

  // Prop tests
  val sIntList = Gen.listOf(Gen.choose(0,100))

  // Valid prop
  Prop.run(
    Prop.forAll (sIntList) {
      ns => ns.reverse.reverse == ns
    } && Prop.forAll (sIntList) {
      ns => ns.headOption == ns.reverse.lastOption
    })

  // Falsified prop
  Prop.run(
    Prop.forAll (sIntList) {
      ns => ns.reverse.reverse == ns
    } && Prop.forAll (sIntList) {
      ns => ns.reverse == ns
    })

  // Valid prop
  Prop.run(
    Prop.forAll (sIntList) {
      ns => ns.reverse.reverse == ns
    } || Prop.forAll (sIntList) {
      ns => ns.reverse == ns
    })

  // Falsified prop
  Prop.run(
    Prop.forAll (sIntList) {
      ns => ns.reverse == ns
    } || Prop.forAll (sIntList) {
      ns => ns.reverse == ns
    })

  Prop.run(
    Prop.forAll (Gen.listOf(Gen.choose(-10,10))) {
      ns => ns.contains(ns.max)
    })

  Prop.run(
    Prop.forAll (Gen.listOf1(Gen.choose(-10,10))) {
      ns => ns.contains(ns.max)
    })

  Prop.run(
    Prop.forAll (Gen.listOf(Gen.choose(-10,10))) { l =>
      val ls = l.sorted
      l.isEmpty ||
      ls.tail.isEmpty ||
      !l.zip(ls.tail).exists { case (a,b) => a > b }
    })
}
