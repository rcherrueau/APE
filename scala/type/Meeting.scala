import scalaz._
import Scalaz._

// [[http://eed3si9n.com/learning-scalaz/]]

object cypher {
  sealed trait Cypher[A] { def data: A }
}

object hes {
  import cypher.Cypher

  sealed trait HES[A] extends Cypher[A]
  case class HES_enc[A](val data: A) extends HES[A]
  case class HES_eq[A: Equal](val data: A)
      extends HES[A] with Equal[HES[A]] {
    override def equal(x: HES[A], y: HES[A]) = x.data === y.data
  }
  case class HES_ord[A: Order](val data: A)
      extends HES[A] with Order[HES[A]] {
    override def order(x: HES[A], y: HES[A]) = x.data cmp y.data
  }
}

object MeetingsApp extends App {
  import hes._
  import utils._

  object utils {
  trait Stats {
    def count[A: Equal](l: List[A]): List[(A, Int)] = l match {
      case Nil => Nil
      case hd :: tl =>
        (hd, 1 + tl.filter(hd === _).length) ::
        count(tl.filter(hd =/= _))
    }
  }

  def meeting(date: String, name: String, address: String) =
    (date, name, address)
  }

  object Calendar {
    def meetings[D: Order, N: Equal, A](ts: List[(D,N,A)],
                                        date: D,
                                        name: N): List[(D,N,A)] =
      for ((d,n,a) <- ts
        if n === name;
        if d >= date) yield (d,n,a)
  }

  object Stats1 extends Stats {
    def mostVisitedClient[N: Equal](ts: List[(_,N,_)]): N =
      count(ts.map(_._2)).maxBy(_._2)._1
  }

  object Stats2 extends Stats {
    def visitedPlaces[A: Equal](ts: List[(_,_,A)]): List[(A, Int)] =
      count(ts.map(_._3))
  }

  object App1 {
    def apply[D: Order, N: Equal, A: Equal](ts: List[(D,N,A)],
                                            date: D,
                                            name: N) =
      Stats2.visitedPlaces(
        Calendar.meetings(ts, date, name))
  }

  object App2 {
    def apply[N: Equal](ts: List[(_,N,_)]) =
      Stats1.mostVisitedClient(ts)
  }

  object App3 {
    def apply[A: Equal](ts: List[(_,_,A)]) =
      Stats2.visitedPlaces(ts)
  }

  val ts =
    meeting("2014-01-01", "Bob",   "a") ::
    meeting("2014-01-02", "Chuck", "b") ::
    meeting("2014-01-03", "Bob",   "c") ::
    meeting("2014-01-04", "Chuck", "d") ::
    meeting("2014-01-05", "Bob",   "e") ::
    meeting("2014-01-06", "Bob",   "e") ::
    meeting("2014-01-07", "Bob",   "e") ::
    meeting("2014-01-08", "Bob",   "f") ::
    meeting("2014-01-09", "Chuck", "b") ::
    meeting("2014-01-10", "Chuck", "g") :: Nil

  val date = "2014-01-06"

  val name = "Bob"

  println(App1(ts, date, name))
  println(App2(ts))
  println(App3(ts))
}
  // // Homomorphic  Encryption Scheme (HES)
  // trait HES[T] { def data: T }

  // class HESenc[T](val data: T) extends HES[T]

  // //  type HESEquiv[T] = HESenc[T] => HESeq[T]
  // type Eq[T] = T => Equiv[T]
  // class HESeq[T](val data: T) extends HES[T] with Equiv[HESenc[T]] {
  //   def equiv(x: HESenc[T], y: HESenc[T]) = {
  //     println("============= in equiv ::::::::::::::::::")
  //     implicitly[Equiv[T]].equiv(x.data, y.data)
  //   }
  // }

  // type Ord[T] = T => Ordering[T]
  // class HESord[T: Ordering](val data: T) extends HES[T]
  //                                        with Ordering[HESenc[T]] {
  //   def compare(x: HESenc[T], y: HESenc[T]) = {
  //     println("============= in ord  ::::::::::::::::::::")
  //     implicitly[Ordering[T]].compare(x.data, y.data)
  //   }
  // }

  // implicit def HESStringWithEq[T: Equiv](hesenc: HESenc[T]): HESeq[T] =
  //   new HESeq(hesenc.data)

  // implicit def HESStringWithOrd[T: Ordering](hesenc: HESenc[T]): HESord[T] =
  //   new HESord(hesenc.data)


  // // sealed abstract class Hphic[A](data: A)
  // // case class HEnc[A](data: A) extends Hphic(data)
  // // case class HEq[A: Equiv](data: A) extends Hphic(data)
  // // implicit def heqEquiv[A] = new Equiv[HEq[A]] {
  // //   override def equiv(x: HEq[A], y: HEq[A]) =
  // //     implicitly[Equiv[A]].equiv(x.data, y.data)
  // // }
  // // case class HOrd[A: Ordering](data: A) extends Hphic(data)
  // // implicit def heqOrd[A: Ordering] = new Ordering[HOrd[A]] {
  // //   override def compare(x: HOrd[A], y: HOrd[A]): Int =
  // //     implicitly[Ordering[A]].compare(x.data, y.data)
  // // }
  // // implicit def hphicEq[A: Equiv](hphic: HEnc[A]): HEq[A] =
  // //   HEq(hphic.data)
  // // implicit def hphicOrd[A](hphic: HEnc[A])(implicit cmp: Ordering[A]):
  // //     HOrd[A] = HOrd(hphic.data)(cmp)

  // def printCalendar(ts: List[_]) =
  //   println(ts.toString().replaceAll(", ", "\n     "))

  // def testEquiv[T: Eq](n1: T, n2: T) =
  //   n1.equiv(n1,n2)

  // def testOrd[N: Ord](n1: N, n2: N) =
  //   n1.gt(n1,n2)

  // println(testEquiv(new HESenc("a"), new HESenc("a")))
  // println(testOrd(new HESenc("a"), new HESenc("a")))
