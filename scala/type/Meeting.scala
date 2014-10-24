import com.github.nscala_time.time.Imports._

object MeetingsApp extends App {
  implicit def toTuple1[V](value: V): Tuple1[V] = Tuple1(value)
  case class Pipeable[V](value: V) {
    def |>[R](f: V => R) = f(value)
  }
  case class PipeableT3[V,W,X](value: Tuple3[V,W,X]) {
    def |>[R](f: (V,W,X) => R) = Function.tupled(f)(value)
  }
  case class PipeableT4[V,W,X,Y](value: Tuple4[V,W,X,Y]) {
    def |>[R](f: (V,W,X,Y) => R) = Function.tupled(f)(value)
  }
  implicit def toPiped[V](value: V) = Pipeable(value)
  implicit def toPiped[V,W,X](value: Tuple3[V,W,X]) = PipeableT3(value)
  implicit def toPiped[V,W,X,Y](value: Tuple4[V,W,X,Y]) = PipeableT4(value)


  case class Meeting (name: String,
                      date: DateTime,
                      address: String)

  object Calendar {
    def meetings(db: List[Meeting],
                 name: String,
                 date: DateTime): List[Meeting] =
      db.filter(m => m.name == name && m.date >= date)
  }

  object Stats1 {
    def mostVisitedClient(db: List[Meeting]): String =
      (db.foldLeft(Map.empty[String, Int])(
        (visit: Map[String, Int], m: Meeting) => {
          val client = m.name
          visit + (client -> (visit.getOrElse(client, 1) + 1))
        })).maxBy(_._2)._1
  }

  object Stats2 {
    def mostVisitedPlaces(db: List[Meeting]): Map[String, Int] =
      db.foldLeft(Map.empty[String, Int])(
        (visit: Map[String, Int], m: Meeting) => {
          val place = m.address
          visit + (place -> (visit.getOrElse(place, 0) + 1))
        })
  }

  val db =
    Meeting("Bob",   new DateTime(2014, 1, 1, 0, 0), "a") ::
    Meeting("Chuck", new DateTime(2014, 1, 2, 0, 0), "b") ::
    Meeting("Bob",   new DateTime(2014, 1, 3, 0, 0), "c") ::
    Meeting("Chuck", new DateTime(2014, 1, 4, 0, 0), "d") ::
    Meeting("Bob",   new DateTime(2014, 1, 5, 0, 0), "e") ::
    Meeting("Bob",   new DateTime(2014, 1, 6, 0, 0), "e") ::
    Meeting("Bob",   new DateTime(2014, 1, 7, 0, 0), "e") ::
    Meeting("Bob",   new DateTime(2014, 1, 8, 0, 0), "f") ::
    Meeting("Chuck", new DateTime(2014, 1, 9, 0, 0), "b") ::
    Meeting("Chuck", new DateTime(2014, 1, 10, 0, 0), "g") :: Nil

  def printCalendar(db: List[Meeting]) = println(db.toString().replaceAll(", ", "\n     "))

  val res1 = Calendar.meetings(db, "Bob", new DateTime(2014, 1, 6, 0, 0))
  printCalendar(res1)
  val res2 = Stats2.mostVisitedPlaces(res1)
  println(res2)
  val res3 = Stats1.mostVisitedClient(db)
  println(res3)
}
