import com.github.nscala_time.time.Imports._

object MeetingsApp extends App {
  type Ord[T] = T => Ordered[T]
  sealed abstract class HEnc[A](data: A)
  case class HEq[A: Equiv](data: A) extends HEnc(data)
  implicit def heqEquiv[A]: Equiv[HEq[A]] = new Equiv[HEq[A]] {
    override def equiv(x: HEq[A], y: HEq[A]) =
      implicitly[Equiv[A]].equiv(x.data, y.data)
  }
  case class HOrd[A: Ord](data: A) extends HEnc(data) with Ordered[HOrd[A]] {
    override def compare(that: HOrd[A]): Int =
      if (data < that.data) -1
      else if (data == that.data) 0
      else 1
  }
  object MakeH {
    def enc[A: Equiv](a: A): HEq[A] = HEq(a)
    def enc[A: Ord](a: A): HOrd[A] = HOrd(a)
  }


  object Calendar {
    def meetings[D: Ord, N: Equiv](ts: List[(D,N,_)], name: N, date: D) =
      for ((d,n,a) <- ts
        if implicitly[Equiv[N]].equiv(n, name);
        if d >= date) yield (d,n,a)
  }

  object Stats1 {
    def mostVisitedClient[N](ts: List[(_,N,_)]) =
      (ts.foldLeft(Map.empty[N, Int])(
        (visit: Map[N, Int], t: (_,N,_)) => {
          val client = t._2
          visit + (client -> (visit.getOrElse(client, 1) + 1))
        })).maxBy(_._2)._1
  }

  object Stats2 {
    def mostVisitedPlaces[A](ts: List[(_,_,A)]) =
      ts.foldLeft(Map.empty[A, Int])(
        (visit: Map[A, Int], t: (_,_,A)) => {
          val place = t._3
          visit + (place -> (visit.getOrElse(place, 0) + 1))
        })
  }

  def meeting(date: DateTime, name: String, address: String) =
    (MakeH.enc(date), MakeH.enc(name), address)

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

  val res1 = Calendar.meetings(ts, MakeH.enc("Bob"), MakeH.enc(new DateTime(2014, 1, 6, 0, 0)))
  printCalendar(res1)
  val res2 = Stats2.mostVisitedPlaces(res1)
  println(res2)
  val res3 = Stats1.mostVisitedClient(ts)
  println(res3)
}
