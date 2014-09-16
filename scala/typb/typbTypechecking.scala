// http://danielwestheide.com/blog/2013/02/13/the-neophytes-guide-to-scala-part-13-path-dependent-types.html
// http://stackoverflow.com/a/12937819
// https://github.com/milessabin/shapeless

// The view implicit! Next step, pass the view implicitely

object Cloud {
  import collection.mutable.Map

  abstract class View(id: Long) { type Nature }
  trait RawView extends View { type Nature = Raw }
  trait EncryptedView extends View { type Nature = Encrypted }

  object ViewMaker {
    import scala.util.Random
    object Raw {
      def apply() = new View(Random.nextLong()) with RawView
    }
    object Encrypted {
      def apply() = new View(Random.nextLong()) with EncryptedView
    }
  }

  val database = Map.empty[View, Any]

  def read(view: View): Option[view.Nature] =
    database.get(view).asInstanceOf[Option[view.Nature]]

  def store(data: Nature)(implicit view: View): view.type = {
    database.update(view, data)
    view
  }
}

sealed abstract class Nature
case class Raw extends Nature
case class Encrypted extends Nature

object TYPB_TypeChecking {
  def main(args: Array[String]) {
    import Cloud.View
    import Cloud.RawView
    import Cloud.ViewMaker

    val rawView: RawView = ViewMaker.Raw()
    val t = Cloud.store(Raw())(rawView)
    val rawData: Option[Raw] = Cloud.read(t)

    // val encView = ViewMaker.Encrypted()
    // // Cloud.store(encView)(Raw()) // Doesn't compile
    // Cloud.store(encView)(Encrypted())
    // // val rawData: Option[Raw] = Cloud.read(encView) // Doesn't compile
    // val encData: Option[Encrypted] = Cloud.read(encView)
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
