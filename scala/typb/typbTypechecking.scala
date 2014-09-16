// http://danielwestheide.com/blog/2013/02/13/the-neophytes-guide-to-scala-part-13-path-dependent-types.html
// http://stackoverflow.com/a/12937819
// https://github.com/milessabin/shapeless

// With, dependate data I have to find a new way to do the read and
// store.
abstract class Nature
case class Raw[+T](value: T) extends Nature
case class Encrypted extends Nature
case class Pull extends Nature

trait Data[+T <: Nature] { def get: T }

abstract class Key[T](id: Long) { type DataType }

trait Id {
  import scala.util.Random

  case class IdData[+T <: Nature](get: T) extends Data[T]

  def makeKey[T <: Nature](data: Data[T]): Key[IdData[T]] =
    new Key[IdData[T]](Random.nextLong()) { type DataType = IdData[T] }
  def encrypt(data: Data[Nature]): IdData[Encrypted]
  def decrypt(data: IdData[Encrypted]): Data[Nature]
}

case object Alice extends Id {
  def encrypt(data: Data[Nature]): IdData[Encrypted] = null
  def decrypt(data: IdData[Encrypted]): Data[Nature] = null
}

case object Bob extends Id {
  def encrypt(data: Data[Nature]): IdData[Encrypted] = null
  def decrypt(data: IdData[Encrypted]): Data[Nature] = null
}

object Cloud {
  import annotation.implicitNotFound
  import scala.util.Random
  import collection.mutable.Map

  val database = Map.empty[Any, Any]

  def store[T <: Nature](id: Id)(data: Data[T]): Key[id.IdData[T]] = {
    val key = id.makeKey(data)
    database.update(key, data)
    key
  }

  def read[T <: Nature](id: Id)(key: Key[id.IdData[T]]): Option[id.IdData[T]] =
    database.get(key).asInstanceOf[Option[id.IdData[T]]]

  // Should accept all Nature except Encrypted
  def genChart[T](data: T)
    (implicit an: GenChartAcceptNature[T]): Unit = {}
  @implicitNotFound("Type mismatch ${T} is forbidden")
  case class GenChartAcceptNature[T]
  implicit object AcceptRaw extends GenChartAcceptNature[Alice.IdData[Raw[String]]]
  implicit object AcceptPull extends GenChartAcceptNature[Alice.IdData[Pull]]
}

object TYPB_TypeChecking {
  // import Cloud.RawView
  // import Cloud.EncryptedView

  def testDecryptNotYours {
    Alice.decrypt(Alice.encrypt(new Alice.IdData(Raw("test"))))
    Alice.decrypt(Alice.encrypt(new Bob.IdData(Raw("test"))))
    // Doesn't compile: Bob couldn't decrypt Alice ecrypted data
    // Bob.decrypt(Alice.encrypt(new Bob.IdData(Raw("test"))))
  }

  def testAllExceptEncrypted {
    Cloud.genChart(new Alice.IdData(Raw("test")))
    Cloud.genChart(new Alice.IdData(Pull()))
    // Doesn't compile: Encrypted is forbidden in genChart
    // Cloud.genChart(new Alice.IdData(Encrypted()))
  }

  def testReturnGoodType {
    val key = Cloud.store(Alice)(new Alice.IdData(Raw("test")))
    val data: Option[Alice.IdData[Raw[String]]] = Cloud.read(Alice)(key)

    // Doesn't compile: Bob couldn't get Alice data
    // val badData1: Option[Bob.IdData[Raw[String]]] = Cloud.read(Bob)(key)

    // Doesn't compile: the type of badData is not good
    // val badData2: Option[Alice.IdData[Pull]] = Cloud.read(Alice)(key)
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
