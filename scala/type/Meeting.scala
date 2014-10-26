import com.github.nscala_time.time.Imports._

trait Stats {
  def count[A: Equiv](l: List[A]): List[(A, Int)] = l match {
    case Nil => Nil
    case hd :: tl =>
      (hd, 1 + tl.filter(implicitly[Equiv[A]].equiv(hd, _)).length) ::
      count(tl.filter(!implicitly[Equiv[A]].equiv(hd, _)))
  }
}

object MeetingsApp extends App {
  type Ord[T] = T => Ordered[T]

  sealed abstract class Hphic[A](data: A)
  case class HEnc[A](data: A) extends Hphic(data)
  case class HEq[A: Equiv](data: A) extends Hphic(data)
  implicit def heqEquiv[A]: Equiv[HEq[A]] = new Equiv[HEq[A]] {
    override def equiv(x: HEq[A], y: HEq[A]) =
      implicitly[Equiv[A]].equiv(x.data, y.data)
  }
  case class HOrd[A: Ord](data: A) extends Hphic(data) with Ordered[HOrd[A]] {
    override def compare(that: HOrd[A]): Int =
      if (data < that.data) -1
      else if (data == that.data) 0
      else 1
  }
  implicit def hphicEq[A: Equiv](hphic: HEnc[A]): HEq[A] =
    HEq(hphic.data)
  implicit def hphicOrd[A: Ord](hphic: HEnc[A]): HOrd[A] =
    HOrd(hphic.data)

  object Calendar {
    def meetings[D: Ord, N: Equiv]
        (ts: List[(D,N,_)], name: N, date: D): List[(D,N,_)] =
      for ((d,n,a) <- ts
        if implicitly[Equiv[N]].equiv(n, name);
        if d >= date) yield (d,n,a)
  }

  object Stats1 extends Stats {
    def mostVisitedClient[N](ts: List[(_,N,_)]): N =
      count(ts.map(_._2)).maxBy(_._2)._1
  }

  object Stats2 extends Stats {
    def mostVisitedPlaces[A](ts: List[(_,_,A)]) =
      count(ts.map(_._3))
  }

  def meeting(date: DateTime, name: String, address: String) =
    (HEnc(date), HEnc(name), address)

  val ts =
    meeting(new DateTime(2014, 1, 1, 0, 0),  "Bob",   "a") ::
    meeting(new DateTime(2014, 1, 2, 0, 0),  "Chuck", "b") ::
    meeting(new DateTime(2014, 1, 3, 0, 0),  "Bob",   "c") ::
    meeting(new DateTime(2014, 1, 4, 0, 0),  "Chuck", "d") ::
    meeting(new DateTime(2014, 1, 5, 0, 0),  "Bob",   "e") ::
    meeting(new DateTime(2014, 1, 6, 0, 0),  "Bob",   "e") ::
    meeting(new DateTime(2014, 1, 7, 0, 0),  "Bob",   "e") ::
    meeting(new DateTime(2014, 1, 8, 0, 0),  "Bob",   "f") ::
    meeting(new DateTime(2014, 1, 9, 0, 0),  "Chuck", "b") ::
    meeting(new DateTime(2014, 1, 10, 0, 0), "Chuck", "g") :: Nil

  def printCalendar(ts: List[_]) =
    println(ts.toString().replaceAll(", ", "\n     "))

  println(Stats1.mostVisitedClient(ts))

  val res1 = Calendar.meetings(ts, HEnc("Bob"), HEnc(new DateTime(2014, 1, 6, 0, 0)))
  printCalendar(res1)

  val res2 = Stats2.mostVisitedPlaces(ts)
  println(res2)

}
