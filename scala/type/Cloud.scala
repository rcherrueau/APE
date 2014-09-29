// TODO: Check the not type parameter
// https://groups.google.com/d/msg/scala-language/4SQt-n1l9Zk/jIgzYrJiMNMJ

trait Id {
  sealed abstract class IdData
  sealed abstract class Encrypted extends IdData
  final case class Raw() extends IdData
  final case class Sany() extends IdData
  final case class AES_ECS[+T <: Data](data: T) extends Encrypted


  // Refer all kind of Data without considering path dependant.
  type Data = Id#IdData

  // // Encoding for "A is not a subtype of B"
  // trait <:!<[A, B]


  // // Uses ambiguity to rule out the cases we're trying to exclude
  // implicit def nsub[A, B] : A <:!< B = null
  // implicit def nsubAmbig1[A, B >: A] : A <:!< B = null
  // implicit def nsubAmbig2[A, B >: A] : A <:!< B = null

  // // Type alias for context bound
  // type ¬[T] = {
  //   type λ[α] = α <:!< T
  // }

  @implicitNotFound(msg = "Cannot ${T}.")
  class NotEncrypted[T <: Data](d: T)
  implicit def notEncRaw[T <: Id#Raw]: NotEncrypted[T] = null
  implicit def notEncSany[T <: Id#Sany]: NotEncrypted[T] = null

  def encrypt[T <: Data](d: T): AES_ECS[T] = AES_ECS(d)
  def decrypt[T <: Data](d: AES_ECS[T]): T = d.data
}

object Alice extends Id

object Bob extends Id

object AliceAndBobTests extends App {
  val aliceRaw: Alice.Raw = new Alice.Raw()
  val aliceEncrypted: Alice.AES_ECS[Alice.Raw] = Alice.encrypt(aliceRaw)
  val aliceDecrypted: Alice.Raw = Alice.decrypt(aliceEncrypted)

  val bobRaw: Bob.Raw = new Bob.Raw()
  val bobEncrypted: Bob.AES_ECS[Bob.Raw] = Bob.encrypt(bobRaw)
  val bobDecrypted: Bob.Raw = Bob.decrypt(bobEncrypted)

  Bob.encrypt(aliceRaw)
  // Won't type checks: Bob cannot decrypt a data encrypted by Alice.
  // Bob.decrypt(aliceEncrypted)
}

object DummyCloud extends Id {
  case class Key()
  case class Chart() extends IdData

  // Stores a data and returns an accessor.
  def store(d: Data): Key = ???

  // Takes a data accessor and returns a data.
  def read(k: Key): Data = ???

  // Chart generator that takes any kind of Data.
  def genChart(d: Data): Chart = ???

  // Returns a chart from a data accessor.
  def processData(k: Key): Chart = genChart(read(k))
}

object DummyCloudTests extends App {
  import DummyCloud.Key

  val rawdataKey: Key =
    DummyCloud.store(Alice.Raw())
  val encdataKey: Key =
    DummyCloud.store(Alice.encrypt(Alice.Raw()))

  // Works with raw data - Great!
  DummyCloud.processData(rawdataKey)

  // Works with encrypted data - D'oh!
  DummyCloud.processData(encdataKey)
}

// T encode the type of the stored data
object SmarterCloud extends Id {
  case class Key[T <: Data]()
  case class Chart() extends IdData

  // Stores a data and returns an accessor.
  def store[T <: Data](d: T): Key[T] = ???

  // Takes a data accessor and returns a data.
  def read[T <: Data](k: Key[T]): T = ???

  // Chart generator that takes any kind of Data.
  def genChart[T <: Data : NotEncrypted](d: T): Chart = Chart()

  // Returns a chart from a data accessor.
  def processData[T <: Data](id: Id)(k: Key[T]): Chart = {
    // read(k) match {
    //   case d: id.Raw => genChart(d)
    //   // case d: id.Encrypted => genChart(id.Raw())
    // }
    genChart(read(k))
  }
}

object SmarterCloudTests {
  import SmarterCloud.Key
  val Cloud = SmarterCloud

  val data: Alice.Raw = Alice.Raw()
  val encData: Alice.AES_ECS[Alice.Raw] = Alice.encrypt(data)

  Cloud.genChart(data)
  // Cloud.genChart(encData) // Doesn't compile

  val key1 = Cloud.store(data)
  val key2 = Cloud.store(encData)
  val res = Cloud.read(key1)
  val encRes = Cloud.read(key2)

  Cloud.genChart(res)
  // Cloud.genChart(encRes) // Doesn't compile

  Cloud.processData(Alice)(key1)
  Cloud.processData(Alice)(key2)
}
// // genChart that takes any kind of Data except Encrypted ones
// def genChart2[T >: Id#Encrypted[_] <: Data](d: T): Chart = ???

// getCals that uses genChart1
// def getCals1[T] = {
//   val readedData: key. = read(key)

// }
