/** Functional Programming in Scala, Chapter 4 */
package fpinscala

sealed trait Option[+A] {
  /** Apply f if the option is not None.
    *
    * Proceed with a computation on the assumption that an error
    * hasn't occured.
    */
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  /** Apply f, which may fail, to the Option if not None. */
  def flatMap[B](f: A => Option[B]): Option[B] =
    this map(f) getOrElse None

  /** Returns a default value if the option is None. */
  def getOrElse[B >: A](default: => B): B = this match {
    // the `default: => B' indicates that the argument is of type B,
    // but won't by evaluated until it's needed by the function.
    case Some(b) => b
    case None => default
  }

  /** Returns a default option if the option is None. */
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    // the `ob: => B' indicates that the argument is of type B, but
    // won't by evaluated until it's needed by the function.
    case s@Some(_) => s
    case None => ob
  }

  /** Convert Some to None if the value doesn't satisfy f. */
  def filter(f: A => Boolean): Option[A] =
    this flatMap { a => if (f(a)) Some(a) else None }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def lift[A,B](f: A => B): Option[A] => Option[B] =
    (x: Option[A]) => x map f

  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A,B) => C): Option[C] =
    oa flatMap (a => ob map (b => f(a,b)))

  def sequence[A](l: List[Option[A]]): Option[List[A]] =
    l.foldRight(Some(Nil:List[A]):Option[List[A]])(
      (h,rest) => h flatMap (x => rest map (x :: _)))

  def sequence_2[A](l: List[Option[A]]): Option[List[A]] =
    l.foldRight[Option[List[A]]](Some(Nil))(
      (h,rest) => map2(h,rest)(_ :: _))

  def sequence_3[A](l: List[Option[A]]): Option[List[A]] =
    l.foldRight[Option[List[A]]](Some(Nil))(
      (h, rest) => for {
        hh <- h
        rr <- rest
      } yield hh :: rr)

  def traverse[A,B](l: List[A])(f: A => Option[B]): Option[List[B]] =
    l.foldRight(Some(Nil:List[B]):Option[List[B]])(
      (h,rest) => f(h) flatMap (x => rest map (x :: _)))

  def traverse_2[A,B](l: List[A])(f: A => Option[B]): Option[List[B]] =
    l.foldRight[Option[List[B]]](Some(Nil))(
      // Use for-comprehensions
      (h,t) => for {
        hh <- f(h)
        tt <- t
      } yield (hh :: tt))
}

object FPInScalaOptionTest extends App {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m => mean(xs.map(x => math.pow(x - m, 2))) }

  def variance_2(xs: Seq[Double]): Option[Double] = {
    val means: Option[Seq[Double]] =
      mean(xs) map (m => xs.map(x => math.pow(x - m, 2)))

    means flatMap mean
  }

  def insuranceRateQuote(age: Int,
                         numberOfSpeedingTicket: Int): Double =
    (age + numberOfSpeedingTicket) / .42

  def parseInsuranceRateQuote(age: String,
                              numberOfSpeedingTicket: String):
      Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTicket.toInt)

    // insuranceRateQuote(optAge, optTickets) // Doesn't type check

    // Option.lift(insuranceRateQuote _)(optAge,
    //                                   optTickets) // doesn't type
    //                                               // check,
    //                                               // insuranceRateQuote
    //                                               // takes two
    //                                               // arguments

    Option.map2(optAge, optTickets)(insuranceRateQuote _)
  }

  def parseInts(l: List[String]): Option[List[Int]] =
    Option.sequence(l map (x => Try(x.toInt)))

  def parseInts_2(l: List[String]): Option[List[Int]] =
    Option.traverse(l)(x => Try(x.toInt))

  def parseInts_3(l: List[String]): Option[List[Int]] =
    Option.traverse_2(l)(x => Try(x.toInt))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  println(mean(Seq(1., 2., 3., 4., 5.)))
  println(mean(Seq()))
  println(variance(Seq(1., 2., 3., 4., 5.)))
  println(variance(Seq()))
  println(variance_2(Seq(1., 2., 3., 4., 5.)))
  println(variance_2(Seq()))
  println(Option.sequence(List(Some(1),Some(2),Some(3))))
  println(Option.sequence(List(Some(1),None,Some(3))))
  println(Option.traverse(List("1", "2", "3"))(Some(_)))
  println(Option.traverse_2(List("1", "2", "3"))(Some(_)))
  println(parseInsuranceRateQuote("25", "100"))
  println(parseInsuranceRateQuote("25", "a"))
  println(parseInsuranceRateQuote("a", "100"))
  println(parseInts(List("1", "2", "3")))
  println(parseInts(List("1", "a", "3")))
  println(parseInts_2(List("1", "2", "3")))
  println(parseInts_2(List("1", "a", "3")))
  println(parseInts_3(List("1", "2", "3")))
  println(parseInts_3(List("1", "a", "3")))
}
