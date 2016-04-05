import com.github.nscala_time.time.Imports._

object CalsApp extends App {
  case class Chart()
  case class GPS()
  case class Activity(id: String,
                      date: DateTime,
                      bcals: Int,
                      duration: Interval =  new Interval(0,0),
                      circuit: List[GPS] = Nil)

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


  // Rendre l'algo polymorphe pour faire apparaître les opération dans le typage.
  def getBurntCals(db: List[Activity], year: Int, id: String) =
    Tuple4(db, new DateTime(year, 1, 1, 0, 0), new DateTime(year + 1, 12, 31, 23, 59), id)

  def getActivities(db: List[Activity], start: DateTime, end: DateTime, id: String) =
    db.filter((act: Activity) => (act.id == id) && ((act.date >= start) || (act.date <= end)))

  def sumBurntCalsPerMonth(activities: List[Activity]) =
      activities.foldLeft(Map.empty[String, Int])(
        (calsPerMonth: Map[String, Int], activity: Activity) => {
          val month = activity.date.month.getAsText()
          calsPerMonth + (month -> (calsPerMonth.getOrElse(month, 0) + activity.bcals))
       })

  def genChart(calsPerMonth: Map[String, Int]) =
    calsPerMonth.toString().replaceAll(", ", "\n    ")

  val db: List[Activity] =
      Activity("Alice", new DateTime(2014, 1, 1, 0, 0), 1) ::
      Activity("Alice", new DateTime(2014, 2, 1, 0, 0), 1) ::
      Activity("Alice", new DateTime(2014, 3, 1, 0, 0), 1) ::
      Activity("Alice", new DateTime(2014, 4, 1, 0, 0), 1) ::
      Activity("Alice", new DateTime(2014, 5, 1, 0, 0), 1) ::
      Activity("Alice", new DateTime(2014, 6, 1, 0, 0), 1) ::
      Activity("Alice", new DateTime(2014, 6, 2, 0, 0), 1) ::
      Activity("Alice", new DateTime(2014, 6, 3, 0, 0), 1) ::
      Activity("Alice", new DateTime(2014, 8, 1, 0, 0), 1) ::
      Activity("Alice", new DateTime(2014, 9, 1, 0, 0), 1) ::
      Activity("Alice", new DateTime(2014, 10, 1, 0, 0), 1) ::
      Activity("Alice", new DateTime(2014, 11, 1, 0, 0), 1) ::
      Activity("Alice", new DateTime(2014, 12, 1, 0, 0), 1) :: Nil

  getBurntCals(db, 2014, "Alice") |> getActivities |> sumBurntCalsPerMonth |> genChart |> println
}
