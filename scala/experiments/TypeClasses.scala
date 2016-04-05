/** The Neophyte's Guide to Scala Part 12: Type Classes
  *
  * [[http://danielwestheide.com/blog/2013/02/06/the-neophytes-guide-to-scala-part-12-type-classes.html]]
  */

/** Fancy statistics library: No polymorphisme */
object StatisticsNoPolymorphisme {
  def median(xs: Vector[Double]): Double = xs(xs.size / 2)
  def quartiles(xs: Vector[Double]): (Double, Double, Double) =
    (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))
  def iqr(xs: Vector[Double]): Double = quartiles(xs) match {
    case (lowerQuartile, _, upperQuartile) => upperQuartile - lowerQuartile
  }
  def mean(xs: Vector[Double]): Double = xs.reduce(_ + _) / xs.size
}

// Now we want to support more than just double numbers. So let's
// implement all these methods again for Int numbbers, right? No, the
// type parameter suffers from type erasure!
//
//   def median(xs: Vector[Int]): Int = xs(xs.size / 2)
//   def quartiles(xs: Vector[Int]): (Int, Int, Int) =
//     (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))
//
// > [error] method quartiles:(xs: Vector[Int])(Int, Int, Int) and
// > [error] method quartiles:(xs: Vector[Double])(Double, Double,
// > Double) at line 9
// > [error] have same type after erasure: (xs: collection.immutab
// > le.Vector)Tuple3

// As a solution, we could use subtype polymorphisme. If scala offers
// a subtype `Number' such that Number :> Double and Number :> Int,
// the library written to take a Number will work equally well when
// passed an Int or Double as when passed a Number.
//
// /** Fancy statistics library: Subtype polymorphisme */
// object StatisticsPolymorphisme {
//   def median(xs: Vector[Number]): Number = xs(xs.size / 2)
//   def quartiles(xs: Vector[Number]): (Number, Number, Number) =
//     (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))
//   def iqr(xs: Vector[Number]): Number = quartiles(xs) match {
//     case (lowerQuartile, _, upperQuartile) => upperQuartile - lowerQuartile
//   }
//   def mean(xs: Vector[Number]): Number = {
//     xs.reduce(_ + _) / xs.size
//   }
// }
//
// But there is no such common trait in scala.
//
// Java developer solution is Adapter pattern.

/** Fancy statistics library: Adapter pattern */
object StatisticsAdapterPattern {
  trait NumberLike[A] {
    def get: A
    def plus(y: NumberLike[A]): NumberLike[A]
    def minus(y: NumberLike[A]): NumberLike[A]
    def divide(y: Int): NumberLike[A]
  }

  case class NumberLikeDouble(x: Double) extends NumberLike[Double] {
    def get: Double = x
    def minus(y: NumberLike[Double]) = NumberLikeDouble(x - y.get)
    def plus(y: NumberLike[Double]) = NumberLikeDouble(x + y.get)
    def divide(y: Int) = NumberLikeDouble(x / y)
  }

  case class NumberLikeInt(x: Int) extends NumberLike[Int] {
    def get: Int = x
    def minus(y: NumberLike[Int]) = NumberLikeInt(x - y.get)
    def plus(y: NumberLike[Int]) = NumberLikeInt(x + y.get)
    def divide(y: Int) = NumberLikeInt(x / y)
  }

  def median[A](xs: Vector[NumberLike[A]]): NumberLike[A] = xs(xs.size / 2)
  def quartiles[A](xs: Vector[NumberLike[A]]):
      (NumberLike[A], NumberLike[A], NumberLike[A]) =
    (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))
  def iqr[A](xs: Vector[NumberLike[A]]): NumberLike[A] = quartiles(xs) match {
    case (lowerQuartile, _, upperQuartile) => upperQuartile.minus(lowerQuartile)
  }
  def mean[A](xs: Vector[NumberLike[A]]): NumberLike[A] =
    xs.reduce(_.plus(_)).divide(xs.size)
}

// Adapter is OK but two problems left:
// - User of the library has to create instances of the adapter to
//   interact with the library.
// - User of the library has to pass a `NumberLike' which is tiresome
//   to wrtie and read.

object Math {
  // Interface with common operations on Number.
  trait NumberLike[T] {
    def plus(x: T, y: T): T
    def divide(x: T, y: Int): T
    def minus(x: T, y: T): T
  }
  object NumberLike {
    // Rule to apply operation on Double
    implicit object NumberLikeDouble extends NumberLike[Double] {
      def plus(x: Double, y: Double): Double = x + y
      def divide(x: Double, y: Int): Double = x / y
      def minus(x: Double, y: Double): Double = x - y
    }
    // Rule to apply operation on Int
    implicit object NumberLikeInt extends NumberLike[Int] {
      def plus(x: Int, y: Int): Int = x + y
      def divide(x: Int, y: Int): Int = x / y
      def minus(x: Int, y: Int): Int = x - y
    }
  }
}

object StatisticsTypeClass {
  // We put into StatisticsTypeClass's scope implicite rules of
  // Math.NumberLike
  import Math.NumberLike

  // The implicit indicates that if no `NumberLike[T]' is passes. Then
  // scala looks into the global scope.
  def mean[T](xs: Vector[T])(implicit ev: NumberLike[T]): T =
    ev.divide(xs.reduce(ev.plus(_, _)), xs.size)

  def median[T](xs: Vector[T]): T = xs(xs.size / 2)

  def quartiles[T](xs: Vector[T]): (T, T, T) =
    (xs(xs.size / 4), median(xs), xs(xs.size / 4 * 3))

  def iqr[T](xs: Vector[T])(implicit ev: NumberLike[T]): T =
    quartiles(xs) match {
      case (lowerQuartile, _, upperQuartile) =>
        ev.minus(upperQuartile, lowerQuartile)
    }

  def main(args: Array[String]) {
    val strs = Vector("1", "2", "3", "4")
    println(StatisticsTypeClass.quartiles(strs))
  }
}
