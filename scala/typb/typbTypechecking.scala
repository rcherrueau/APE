// http://danielwestheide.com/blog/2013/02/13/the-neophytes-guide-to-scala-part-13-path-dependent-types.html
// http://stackoverflow.com/a/12937819
// https://github.com/milessabin/shapeless

// The program enventualy type checks! Next step is in store, try make
// the view implicit: there is no way to do that!

object Cloud {
  import collection.mutable.Map
  object ViewMaker {
    import scala.util.Random
    object Raw {
      def apply() = new View(Random.nextLong()) { type Nature = Raw }
    }
    object Encrypted {
      def apply() = new View(Random.nextLong()) { type Nature = Encrypted }
    }
  }

  abstract class View(id: Long) { type Nature }

  val database = Map.empty[View, Any]

  def read(view: View): Option[view.Nature] =
    database.get(view).asInstanceOf[Option[view.Nature]]

  def store(view: View)(data: view.Nature): Unit =
    database.update(view, data)
}

sealed abstract class Nature
case class Raw extends Nature
case class Encrypted extends Nature

object TYPB_TypeChecking {
  def main(args: Array[String]) {
    import Cloud.ViewMaker

    val rawView = ViewMaker.Raw()
    Cloud.store(rawView)(Raw())
    val rawData: Option[Raw] = Cloud.read(rawView)

    val encView = ViewMaker.Encrypted()
    // Cloud.store(encView)(Raw()) // Doesn't compile
    Cloud.store(encView)(Encrypted())
    // val rawData: Option[Raw] = Cloud.read(encView) // Doesn't compile
    val rawData: Option[Encrypted] = Cloud.read(encView)
  }

  // def scenario1 {
  //   val data = Alice getPrivate ;
  //   Cloud store data
  //   Cloud getCals1 Alice
  // }

  // // Dosn't typecheck because of encrypt. The Cloud.genChart doesn't
  // // accept encrypted data
  // def scenario2 {
  //   val data = Alice encrypt (Alice getPrivate)
  //   Cloud store data
  //   Cloud getCals1 Alice
  // }

  // def scenario3 {
  //   val data = Alice encrypt (Alice getPrivate)
  //   Cloud store data
  //   Cloud getCals2 Alice
  // }

  // def scenario4 {
  //   val data = Bob encrypt (Alice getPrivate)
  //   Cloud store data
  //   Cloud getCals2 Alice
  // }
}
