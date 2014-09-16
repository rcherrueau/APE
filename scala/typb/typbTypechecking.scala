// http://danielwestheide.com/blog/2013/02/13/the-neophytes-guide-to-scala-part-13-path-dependent-types.html
// http://stackoverflow.com/a/12937819
// https://github.com/milessabin/shapeless

// The view implicit! Next step, pass the view implicitely: No way!

sealed abstract class Nature
case class Raw extends Nature
case class Encrypted extends Nature
case class Pull extends Nature

object Cloud {
  import annotation.implicitNotFound
  import scala.util.Random
  import collection.mutable.Map

  abstract class View {type Nature; def id: Long}
  case class RawView(id: Long) extends View { type Nature = Raw }
  case class EncryptedView(id: Long) extends View { type Nature = Encrypted }
  case class PullView(id: Long) extends View { type Nature = Pull }

  object RawView { def apply() = new RawView(Random.nextLong()) }
  object EncryptedView { def apply() = new EncryptedView(Random.nextLong()) }
  object PullView { def apply() = new PullView(Random.nextLong()) }

  val database = Map.empty[View, Any]

  def read(view: View): Option[view.Nature] =
    database.get(view).asInstanceOf[Option[view.Nature]]

  def store(view: View)(data: view.Nature): view.type = {
    database.update(view, data)
    view
  }

  // Should accept all Nature except Encrypted
  def genChart[T <: Nature](data: T)
    (implicit an: GenChartAcceptNature[T]): Unit = {}
  @implicitNotFound("Type mismatch: ${T} is forbidden")
  case class GenChartAcceptNature[T]
  implicit object AcceptRaw extends GenChartAcceptNature[Raw]
  implicit object AcceptPull extends GenChartAcceptNature[Pull]
}

object TYPB_TypeChecking {
  import Cloud.RawView
  import Cloud.EncryptedView

  def testDependentData {
    val rawView: RawView = RawView()
    Cloud.store(rawView)(Raw())
    val rawData: Option[Raw] = Cloud.read(rawView)

    val encView = EncryptedView()
    // Cloud.store(encView)(Raw()) // Doesn't compile
    Cloud.store(encView)(Encrypted())
    // val rawData: Option[Raw] = Cloud.read(encView) // Doesn't compile
    val encData: Option[Encrypted] = Cloud.read(encView)
  }

  def testAllExceptEncrypted {
    Cloud.genChart(Raw())
    Cloud.genChart(Pull())
    // Cloud.genChart(Encrypted()) // Doesn't compile
  }

  // // Everithing is raw data: The program type checks
  // def scenario1 {
  //   val data = Alice getPrivate ;
  //   Cloud store data
  //   Cloud getCals1 Alice
  // }

  // // The Cloud.genChart doesn't accept encrypted data: The program
  // // doesn't type check arround getCals1
  // def scenario2 {
  //   val data = Alice encrypt (Alice getPrivate)
  //   Cloud store data
  //   Cloud getCals1 Alice
  // }

  // // In Cloud.getCals2 Alice tries to decrypt Bob encrypted data: The
  // // program doesn't type check.
  // def scenario3 {
  //   val data = Bob encrypt (Alice getPrivate)
  //   Cloud store data
  //   Cloud getCals2 Alice
  // }

  // // In Cloud.getCals2 Alice decrypts her data before calling
  // // Cloud.genChart: The program type checks.
  // def scenario4 {
  //   val data = Alice encrypt (Alice getPrivate)
  //   Cloud store data
  //   Cloud getCals2 Alice
  // }
  def main(args: Array[String]) {
  }
}
