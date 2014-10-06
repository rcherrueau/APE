package object privacysafer {
  // A way to encode each technques in the types system is with
  // labels. For each technique we define two traits that specifie if the
  // data was transformed with or without the tecnhique.
  sealed trait NotEncrypted
  sealed trait Encrypted

  trait Id {
    sealed abstract class IdData
    final case class Raw() extends IdData with NotEncrypted
    final case class AESECS[+T <: Data](data: T) extends IdData with Encrypted

    // Refer all kind of Data without considering path dependency.
    type Data = Id#IdData

    def encrypt[T <: Data](d: T): AESECS[T] = AESECS(d)
    def decrypt[T <: Data](d: AESECS[T]): T = d.data
  }
}

import privacysafer._

object Alice extends Id

object Bob extends Id

/** Encryption tests from Alice and Bob */
object AliceAndBobTests extends App {
  // Alice generates data, then encrypt and decrypt it
  val aliceRaw = Alice.Raw()
  Alice.decrypt(Alice.encrypt(aliceRaw))

  // Bob generates data, then encrypt and decrypt it
  val bobRaw = Bob.Raw()
  Bob.decrypt(Bob.encrypt(bobRaw))

  // Bob encrypt alice data
  Bob.encrypt(aliceRaw)
  // Won't type checks: Bob cannot decrypt a data encrypted by Alice.
  // Bob.decrypt(aliceEncrypted)
}

/** DummyCloud
  *
  * DummyCloud is a Cloud which offers three services:
  * 1. Storing data in the Cloud.
  * 2. Reading data from the Cloud.
  * 3. Process data to generate a Chart. This service use an internal
  *    service called `genChart'.
  */
object DummyCloud extends Id {
  case class Key()
  case class Chart() extends IdData

  // Stores a data and returns an accessor.
  def store(d: Data): Key = ???

  // Takes a data accessor and returns a data.
  def read(k: Key): Data = ???

  // Chart generator that takes any kind of Data. This method is
  // private and should be call from `processData'.
  private def genChart(d: Data): Chart = ???

  // Returns a chart from a data accessor.
  def processData(k: Key): Chart = genChart(read(k))
}

/** DummyCloud tests
  *
  * The test stores Alice's raw and encrypted data and then tries to
  * process it. The process type checks with raw and encrypted data
  * which is not exactly what we want. We want that genChart only
  * processes non encrypted data and thus, type checks only in th
  * presene of non encypted data.
  */
object DummyCloudTests extends App {
  import DummyCloud.Key

  // Storing of Raw and Encrypted data
  val rawdataKey = DummyCloud.store(Alice.Raw())
  val encdataKey = DummyCloud.store(Alice.encrypt(Alice.Raw()))

  // Type checks with raw data -- Great!
  DummyCloud.processData(rawdataKey)

  // Type checks with encrypted data -- D'oh!
  DummyCloud.processData(encdataKey)
}

/** SmartCloud
  *
  * SmartCloud is a Cloud which offers three services:
  * 1. Storing data in the Cloud.
  * 2. Reading data from the Cloud.
  * 3. Process data to generate a Chart. This service use an internal
  *    service called `genChart'.
  */
object SmarterCloud extends Id {
  // T encode the type of the stored data
  case class Key[T]()
  case class Chart() extends IdData

  // Stores a data and returns a parametric accessor. The parametric
  // accessor is parametred with the type of the data.
  def store[T <: Data](d: T): Key[T] = ???

  // Takes a data accessor and returns a data of types of the
  // accessor's parameters.
  def read[T <: Data](k: Key[T]): T = ???

  // Chart generator that takes any kind of Data.
  private def genChart[T <: Data with NotEncrypted](d: T): Chart = ???

  // Doesn't type checks: the `T' should be a `NotEncrypted'
  // def processData[T <: Data](k: Key[T]): Chart = genChart(read(k))

  // Returns a chart from a data accessor. Here the notation `with
  // NotEncrypted' is mandatory by the type system.
  def processData[T <: Data with NotEncrypted](k: Key[T]): Chart =
    genChart(read(k))
}

/** SmarterCloud tests
  *
  * The test stores Alice's raw and encrypted data and then tries to
  * process it. The process type checks only with raw data.
  */
object SmarterCloudTests {
  import SmarterCloud.Key

  // Storing of Raw and Encrypted data
  val rawdataKey = SmarterCloud.store(Alice.Raw())
  val encdataKey = SmarterCloud.store(Alice.encrypt(Alice.Raw()))

  // Type checkss with raw data - Great!
  SmarterCloud.processData(rawdataKey)

  // Doesn't type check with encrypted data - Great!
  // SmarterCloud.processData(encdataKey)
}

// object SmartestCloud extends Id {
//   // T encode the type of the stored data
//   case class Key[T]()
//   case class Chart() extends IdData

//   // Stores a data and returns a parametric accessor. The parametric
//   // accessor is parametred with the type of the data.
//   def store[T <: Data](d: T): Key[T] = ???

//   // Takes a data accessor and returns a data of types of the
//   // accessor's parameters.
//   def read[T <: Data](k: Key[T]): T = ???

//   // Chart generator that takes any kind of Data.
//   private def genChart[T <: Data with NotEncrypted](d: T): Chart = ???

//   // Doesn't type checks: the `T' should be a `NotEncrypted'
//   // def processData[T <: Data](k: Key[T]): Chart = genChart(read(k))

//   // Returns a chart from a data accessor. Here the notation `with
//   // NotEncrypted' is mandatory by the type system.
//   def processData[T <: Data, U](id: Id)(k: Key[T]): Chart = read(k) match {
//     case e: Data with Encrypted => e match {
//       case d: id.AESECS[U forSome { type U <: Data }] => genChart(id.decrypt(d))
//       case _ => genChart(id.Raw())
//     }
//     case d: Data with NotEncrypted => genChart(d)
//   }
// }
