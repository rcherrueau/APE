/** Towards Scalaz
  *
  * [[http://typelevel.org/blog/2013/10/13/towards-scalaz-1.html]]
  */

object TowardsScalaz {
  // Summing a List of something, Lets start out with Int.
  // def sum(l: List[Int]): Int = l.reduce(_ + _)

  // But what if we want to sum a List[Double]. The code is the same
  // modulo the type parameter.
  def sumDoubles(l: List[Double]): Double = l.reduce(_ + _)

  // Let's make this generic with the help of `Scala.math.Numeric'
  def sumNumerics[A](l: List[A])(implicit A: Numeric[A]): A =
    l.reduce(A.plus(_, _))

  // We can now sum List[Int], List[Double], List[BigInt], and many
  // more. But let's give this a bit more thought - what if we wanted
  // to "sum" a List[String] - that is, we concatenate all the Strings
  // together to create one large String ?
  def sumStrings(l: List[String]): String = l.reduce(_ + _)
  // This looks exactly like summing Int and Doubles! This however does
  // not work with our sumNumeric - there is no (sane) way to define a
  // Numeric[String].

  // Another way to look at this is that we only use the plus method
  // on Numeric, never any of the other methods that also make sense
  // for numeric types.
  trait Addable[A] {
    def plus(x: A, y: A): A
  }

  // object Addable {
  //   implicit def numericIsAddable[A](implicit A: Numeric[A]): Addable[A] =
  //     new Addable[A] {
  //       def plus(x: A, y: A): A = A.plus(x, y)
  //     }

  //   implicit val stringIsAddable: Addable[String] =
  //     new Addable[String] {
  //       def plus(x: String, y: String): String = x + y
  //     }

  //   // Exercise: define an Addable instance for List[A].
  //   implicit def listGenericsIsAddable[A]: Addable[List[A]] =
  //     new Addable[List[A]] {
  //       def plus(x: List[A], y: List[A]): List[A] = x ++ y
  //     }
  // }

  // def sumGenerics[A](l: List[A])(implicit A: Addable[A]): A =
  //   l.reduce(A.plus(_, _))

  // What happens when we pass in an empty List to our summer function
  // though? We get and exception! To prevent this, use foldLeft
  // instead of reduce.
  def sum(l: List[Int]): Int = l.foldLeft(0)(_ + _)

  // It may be tempting to just add a zero method to Addable, but then
  // we may run into the same issues we had with Numeric later on – we
  // don't always need a "zero", sometimes a binary operation is good
  // enough. So instead, let's create an AddableWithZero type class.
  trait AddableWithZero[A] extends Addable[A] {
    def zero: A
  }

  object AddableWithZero {
    implicit def numericIsAddableWithZero[A](implicit A: Numeric[A]):
        AddableWithZero[A] =
      new AddableWithZero[A] {
        def plus(x: A, y: A): A = A.plus(x, y)
        def zero: A = A.zero
      }

    implicit val stringIsAddable: AddableWithZero[String] =
      new AddableWithZero[String] {
        def plus(x: String, y: String): String = x + y
        def zero: String = ""
      }
  }

  def sumGenerics[A](l: List[A])(implicit A: AddableWithZero[A]): A =
    l.foldLeft(A.zero)(A.plus(_, _))

  // AddableWithZero is a Monoid. Here's what sumGeneric looks like in
  // Scalaz land.
  // import scalaz.Monoid
  // import shapeless._

  // def sumGenericsScalazLand[A](l: List[A])(implicit A: Monoid[A]): A =
  //   l.foldLeft(A.zero)(A.append(_,_))
}
