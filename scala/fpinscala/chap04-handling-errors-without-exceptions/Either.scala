/** Functional Programming in Scala, Chapter 4 */
package fpinscala

/** Either that operate on the *Right value*. */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case l@Left(_) => l
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(v) => f(v)
      case Left(v) => Left(v)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case r@Right(_) => r
      case Left(_) => b
    }

  def map2[EE >: E, B, C](e: Either[EE, B])(f: (A,B) => C): Either[EE,C] =
    this flatMap { x => e map { f(x,_) } }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def traverse[E,A,B](l: List[A])(
                      f: A => Either[E,B]): Either[E, List[B]] =
    l.foldRight[Either[E, List[B]]](Right(Nil))(
      (h,rest) => f(h) flatMap { x => rest map (x :: _) })

  def sequence[E,A](l: List[Either[E,A]]): Either[E, List[A]] =
    traverse(l)(x => x)
}

object FPInScalaEitherTest extends App {
  def parseInsuranceRateQuote(age: String,
                              numberOfSpeedingTicket: String):
      Either[Exception, Double] =
    for {
      a <- Try(age.toInt)
      tickets <- Try(numberOfSpeedingTicket.toInt)
      // yield is unsugared to `map' thus we can call
      // `insuranceRateQuote' without lifting it
    } yield insuranceRateQuote(a, tickets)

   def insuranceRateQuote(age: Int,
                          numberOfSpeedingTicket: Int): Double =
    (age + numberOfSpeedingTicket) / .45

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  case class Person(name: Name, age: Age)
  case class Name(val value: String)
  case class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty")
    else Right(Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range")
    else Right(Age(age))

  def makePerson(name: String, age: Int): Either[String, Person] =
    for {
      n <- mkName(name)
      a <- mkAge(age)
    } yield Person(n, a)

  def makePerson_2(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_,_))

  println(makePerson("Alice", 42))
  println(makePerson("Bob", -1))
  println(makePerson("", 42))
}
