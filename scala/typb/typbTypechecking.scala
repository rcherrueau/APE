// http://danielwestheide.com/blog/2013/02/13/the-neophytes-guide-to-scala-part-13-path-dependent-types.html
// http://stackoverflow.com/a/12937819
// https://github.com/milessabin/shapeless

// With, dependate data I have to find a new way to do the read and
// store.
abstract class Nature

trait Id {
  case class Raw extends Nature
  case class Encrypted extends Nature
  case class Pull extends Nature

  def encrypt(data: Nature): Encrypted
  def decrypt(data: Encrypted): Nature
}

case object Alice extends Id {
  def getPrivate: Nature = Raw()
  def encrypt(data: Nature): Encrypted = Encrypted()
  def decrypt(data: Encrypted): Nature = Raw()
}

case object Bob extends Id {
  def getPrivate: Nature = Raw()
  def encrypt(data: Nature): Encrypted = Encrypted()
  def decrypt(data: Encrypted): Nature = Raw()
}

object Cloud {
  import annotation.implicitNotFound
  import scala.util.Random
  import collection.mutable.Map

  // abstract class View {type Nature; def id: Long; def owner: Id}
  // case class RawView(id: Long, owner: Id) extends View {
  //   type Nature = owner.Raw
  // }
  // case class EncryptedView(id: Long, owner: Id) extends View {
  //   type Nature = owner.Encrypted
  // }
  // case class PullView(id: Long, owner: Id) extends View {
  //   type Nature = owner.Pull
  // }

  // object RawView {
  //   def apply(owner: Id) = new RawView(Random.nextLong(), owner)
  // }
  // object EncryptedView {
  //   def apply(owner: Id) = new EncryptedView(Random.nextLong(), owner)
  // }
  // object PullView {
  //   def apply(owner: Id) = new PullView(Random.nextLong(), owner)
  // }

  val database = Map.empty[View, Any]

  def read(view: View): Option[view.Nature] =
    database.get(view).asInstanceOf[Option[view.Nature]]

  def store(view: View)(data: view.Nature): view.type = {
    database.update(view, data)
    view
  }

  // Should accept all Nature except Encrypted
  def genChart[T](data: T)
    (implicit an: GenChartAcceptNature[T]): Unit = {}
  @implicitNotFound("Type mismatch: ${T} is forbidden")
  case class GenChartAcceptNature[T]
  implicit object AcceptRaw extends GenChartAcceptNature[Alice.Raw]
  implicit object AcceptPull extends GenChartAcceptNature[Alice.Pull]
}


object TYPB_TypeChecking {
  // import Cloud.RawView
  // import Cloud.EncryptedView

  def testDecryptNotYours {
    Alice.decrypt(Alice.encrypt(Alice.Raw()))
    Alice.decrypt(Alice.encrypt(Bob.Raw()))
    // Bob.decrypt(Alice.encrypt(Bob.Raw())) // Doesn't compile
  }

  // def testDependentData {
  //   val rawView: RawView = RawView(Alice)
  //   Cloud.store(rawView)(Alice.Raw())
  //   val rawData: Option[Raw] = Cloud.read(rawView)

  //   val encView = EncryptedView()
  //   // Cloud.store(encView)(Raw()) // Doesn't compile
  //   Cloud.store(encView)(Encrypted())
  //   // val rawData: Option[Raw] = Cloud.read(encView) // Doesn't compile
  //   val encData: Option[Encrypted] = Cloud.read(encView)
  // }

  def testAllExceptEncrypted {
    Cloud.genChart(Alice.Raw())
    Cloud.genChart(Alice.Pull())
    // Cloud.genChart(Alice.Encrypted()) // Doesn't compile
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
