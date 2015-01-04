import scala.slick.driver.H2Driver.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object HelloSlick extends App {
  val db = Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver="org.h2.Driver")

  try {
    // Definition of the MEETING table
    class Meeting(tag: Tag) extends Table[(String, String, String)](tag, "MEETING") {
      def date = column[String]("MEET_DATE")
      def name = column[String]("MEET_NAME")
      def address = column[String]("MEET_ADDRESS")
      def * = (date, name, address)
    }
    val meetings = TableQuery[Meeting]

    val setup = Action.seq(
      // Create the tables, including primary and foreign keys
      (meetings.schema).create,

      // Insert some meetings
      meetings += ("2014-01-01", "Bob", "a"),
      meetings += ("2014-01-02", "Chuck", "b"),
      meetings += ("2014-01-03", "Bob", "c"),
      meetings += ("2014-01-04", "Chuck", "d"),
      meetings += ("2014-01-05", "Bob", "e"),
      meetings += ("2014-01-06", "Bob", "e"),
      meetings += ("2014-01-07", "Bob", "e"),
      meetings += ("2014-01-08", "Bob", "f"),
      meetings += ("2014-01-09", "Chuck", "b"),
      meetings += ("2014-01-10", "Chuck", "g")
    )
    Await.result(db.run(setup), Duration.Inf)

    // Read all meetings and print them to the console
    println("Meetings:")
      Await.result(db.run(meetings.result), Duration.Inf).foreach {
        case (date, name, address) =>
          println("  " + date + "\t" + name + "\t" + address)
      }

    // Why not let the database do the string conversion and concatenation?
    val q1 = for(m <- meetings)
             yield LiteralColumn("  ") ++ m.date ++ "\t" ++ m.name ++
               "\t" ++ m.address
               // The first string constant needs to be lifted manually to a LiteralColumn
               // so that the proper ++ operator is found

    println("Meetings:")
    val f = db.stream(q1.result).foreach(println)
      Await.result(f, Duration.Inf)
  } finally db.close
}
