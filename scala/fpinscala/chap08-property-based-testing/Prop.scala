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

trait Gen[A]
object Gen {
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
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
trait Prop {
  type SuccessCount = Int // Help in the readability of the API
  type FailedCase = String

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop): Prop = ???
}


object Prop {
  // Property creator
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object FPInScalaPropTest extends App {

}
