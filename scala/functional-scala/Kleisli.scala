/** A Small Example of Kleisli Arrows.
  *
  * You have functions that take a simple type and return higher
  * kinded types like [[Option]]s or [[List]]s, and you need to
  * compose those functions.
  *
  * [[http://www.casualmiracles.com/2012/07/02/a-small-example-of-kleisli-arrows/]]
  */
object KleisliApp extends App {
  import scalaz._
  import Scalaz._

  // Some methods that take simple types and return higher-kinded types
  def str(x: Int): Option[String] = Some(x.toString)
  def toInt(x: String): Option[Int] = Some(x.toInt)
  def double(x: Int): Option[Double] = Some(x * 2)

  // What I want is: I take a `i: Int', I pass that `i' to `str'. The
  // result is then applied to `toInt' and the result is then applied
  // to `double'. Thus I want a function that does: `str andThen toInt
  // andThen double' where andThen extract the value if this is a
  // [[scala.Some]] to pass the value to next function or stop if this
  // an object [[None]].

  // Compose those functions using pattern matching
  def compose(i: Int): Option[Double] = str(i) match {
    case Some(x) => toInt(x) match {
      case Some(y) => double(y)
      case _ => None
    }
    case _ => None
  }

  // Compose those functions with the idiomatic way ; as a monad
  def composeIdiomatic(i: Int): Option[Double] =
    str(i) flatMap { toInt(_) flatMap { double(_) }}

  // Compose those functions with the comprehension way
  def composeComprehension(i: Int): Option[Double] =
    for {
      x <- str(i)
      y <- toInt(x)
      z <- double(y)
    } yield z

  // Compose those functions with Kleisli Arrows
  def kcompose: Int => Option[Double] =
    Kleisli(str _) >=> Kleisli(toInt _) >=> Kleisli(double _)

  // Tests
  println(compose(1))              // Some(2.0)
  println(composeIdiomatic(1))     // Some(2.0)
  println(composeComprehension(1)) // Some(2.0)
  println(kcompose(1))             // Some(2.0)
}
