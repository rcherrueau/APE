import com.github.nscala_time.time.Imports._

object MeetingsApp extends App {
  case class Meeting(date: DateTime, name: String, address: String)

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
    Meeting(new DateTime(2014, 1, 1, 0, 0),  "Bob",   "a") ::
    Meeting(new DateTime(2014, 1, 2, 0, 0),  "Chuck", "b") ::
    Meeting(new DateTime(2014, 1, 3, 0, 0),  "Bob",   "c") ::
    Meeting(new DateTime(2014, 1, 4, 0, 0),  "Chuck", "d") ::
    Meeting(new DateTime(2014, 1, 5, 0, 0),  "Bob",   "e") ::
    Meeting(new DateTime(2014, 1, 6, 0, 0),  "Bob",   "e") ::
    Meeting(new DateTime(2014, 1, 7, 0, 0),  "Bob",   "e") ::
    Meeting(new DateTime(2014, 1, 8, 0, 0),  "Bob",   "f") ::
    Meeting(new DateTime(2014, 1, 9, 0, 0),  "Chuck", "b") ::
    Meeting(new DateTime(2014, 1, 10, 0, 0), "Chuck", "g") :: Nil

  def printCalendar(db: List[Meeting]) =
    println(db.toString().replaceAll(", ", "\n     "))

  val res1 = Calendar.meetings(db, "Bob", new DateTime(2014, 1, 6, 0, 0))
  printCalendar(res1)
  val res2 = Stats2.mostVisitedPlaces(res1)
  println(res2)
  val res3 = Stats1.mostVisitedClient(db)
  println(res3)
}
