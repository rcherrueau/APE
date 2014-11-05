import spire.algebra._
import spire.implicits._
import spire.syntax._
import scala.runtime._
import utils._

// [[http://eed3si9n.com/learning-scalaz/]]

object cypher {
  sealed trait Cypher[F] { def data: F }
  sealed abstract class CypherEq[CC[X] <: Cypher[X], F: Eq]
      extends Cypher[F] with Eq[CC[F]] {
    def F: Eq[F] = implicitly[Eq[F]]
    override def eqv(x: CC[F], y: CC[F]): Boolean = F.eqv(x.data,y.data)
  }
  sealed abstract class CypherOrd[CC[X] <: Cypher[X], F: Order]
      extends Cypher[F] with Order[CC[F]] {
    def F: Order[F] = implicitly[Order[F]]
    override def compare(x: CC[F], y: CC[F]): Int = F.compare(x.data, y.data)
  }

  object hes {
    sealed trait Hes[A] extends Cypher[A]
    case class HesEq[F: Eq] (val data: F = ???) extends CypherEq[HesEq, F]
    implicit def heseq[F: Eq]: Eq[HesEq[F]] = HesEq[F]()
    case class HesOrd[F: Order] (val data: F = ???) extends CypherOrd[HesOrd, F]
    implicit def hesord[F: Order]: Order[HesOrd[F]] = HesOrd[F]()
    case class HesEnc[F](val data: F) extends Hes[F]
  }

  object aes {
    sealed trait Aes[F] extends Cypher[F]
    case class AesEnc[F](val data: F) extends Aes[F]
  }
}

object MeetingsApp extends App {
  import cypher.hes._
  import cypher.aes._
  import utils._

  object utils {
    trait Stats {
      def count[A: Eq](l: List[A]): List[(A, Int)] = l match {
        case Nil => Nil
        case hd :: tl =>
          (hd, 1 + tl.filter(hd === _).length) ::
          count(tl.filter(hd =!= _))
      }
    }

    def meeting(date: String, name: String, address: String) =
      (date, name, address)
  }

  object Calendar {
    // meetings^{D: Order, N: Eq}
    def meetings[D: Order, N: Eq, A](ts: List[(D,N,A)],
                                     date: D,
                                     name: N): List[(D,N,A)] =
      for ((d,n,a) <- ts
        if n === name;
        if d >= date) yield (d,n,a)
  }

  object Stats1 extends Stats {
    // mostVisitedClient^{N: Eq}
    def mostVisitedClient[N: Eq](ts: List[(_,N,_)]): N =
      count(ts.map(_._2)).maxBy(_._2)._1

    // mostBusyDay^{D: Eq, N: Raw}
    def mostBusyDay[D: Eq, N](ts: List[(D,N,_)]): (D, List[N]) = {
      val mostBusyDay = count(ts.map(_._1)).maxBy(_._2)._1
      (mostBusyDay, ts.filter(_._1 === mostBusyDay).map(_._2))
    }
  }

  object Stats2 extends Stats {
    // mostVisitedClient^{A: Eq}
    def visitedPlaces[A: Eq](ts: List[(_,_,A)]): List[(A, Int)] =
      count(ts.map(_._3))
  }

  object App1 {
    def apply[D: Order, N: Eq, A: Eq](ts: List[(D,N,A)],
                                      date: D,
                                      name: N) =
      Stats2.visitedPlaces(
        Calendar.meetings(ts, date, name))
  }

  object App2 {
    def apply[N: Eq](ts: List[(_,N,_)]) =
      Stats1.mostVisitedClient(ts)
  }

  object App3 {
    def apply[A: Eq](ts: List[(_,_,A)]) =
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

  // Raw data:
  println(App1(ts, date, name))
  println(App2(ts))
  println(App3(ts))

  // HES data:
  println(App1(ts.map(t => (HesOrd(t._1), HesEq(t._2), HesEq(t._3))),
    HesOrd(date), HesEq(name)))
  // Databe elements and reference to these elements must use the same
  // encryption scheme
  illTyped("""
  println(App1(ts.map(t => (HesOrd(t._1), HesEq(t._2), HesEq(t._3))),
    date, name))
  """)
  // You could not encrypt date with HesEq. Date requies an Hes
  // encryption scheme with order.
  illTyped("""
  println(App1(ts.map(t => (HesEq(t._1), HesEq(t._2), HesEq(t._3))),
    HesEq(date), HesEq(name)))
  """)
  println(App2(ts.map(t => (t._1, HesEq(t._2), t._3))))
  println(App3(ts.map(t => (t._1, t._2, HesEq(t._3)))))

  // AES data:
  // AES has no definition for Eq or Order class type.
  illTyped("""
  println(App1(ts.map(t => (AesEnc(t._1), AesEnc(t._2), AesEnc(t._3))),
    AesEnc(date), AesEnc(name)))
  """)
  // For data with no constraint on it, you could use AES.
  println(App2(ts.map(t => (AesEnc(t._1), t._2, AesEnc(t._3)))))
  println(App3(ts.map(t => (AesEnc(t._1), AesEnc(t._2), t._3))))
}
