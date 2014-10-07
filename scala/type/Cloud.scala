import shapeless.test.illTyped

// object testutil {
//   def typeMismatch(found: String, required: String) =
//     "type mismatch;\n  found   : " + found + "\nrequired: " + required
// }

// import testutil._

package object privacysafer {
  // A way to encode each technques in the types system is with
  // labels. For each technique we define two traits that specifie if the
  // data was transformed with or without the tecnhique.
  sealed trait NotEncrypted
  sealed trait Encrypted

  class Id {
    sealed abstract class IdData
    final case class Raw() extends IdData with NotEncrypted
    final case class AESECS[+T <: Data with NotEncrypted](data: T)
        extends IdData with Encrypted

    // Refer all kind of Data without considering path dependency.
    type Data = Id#IdData

    def encrypt[T <: Data with NotEncrypted](d: T): AESECS[T] = AESECS(d)
    def decrypt[T <: Data with NotEncrypted](d: AESECS[T]): T = d.data
  }
}

import privacysafer._

object Alice extends Id

object Bob extends Id

/** Encryption tests from Alice and Bob */
object EncryptionTests {
  def run {
    // Alice is the owner of the Data
    val aliceData: Alice.IdData = Alice.Raw()
    val aliceRawData: Alice.Raw = Alice.Raw()
    val aliceEncData: Alice.AESECS[Alice.Raw] = Alice.encrypt(aliceRawData)
    val aliceDecData: Alice.Raw = Alice.decrypt(aliceEncData)

    // Bob cannot be the owner of the Data
    illTyped("""
    val bobData: Bob.IdData = Alice.Raw()
    """)
    illTyped("""
    val bobRawData: Bob.Raw = Alice.Raw()
    """)
    illTyped("""
    val bobEncData: Bob.AESECS[Alice.Raw] = Alice.encrypt(aliceRawData)
    """)
    illTyped("""
    val encBobData: Alice.AESECS[Bob.Raw] = Alice.encrypt(aliceRawData)
    """)
    illTyped("""
    val bobDecData: Bob.Raw = Alice.decrypt(aliceEncData)
    """)

    // `Id' cannot encrypt already encrypted data
    illTyped("""
    (new Id).encrypt(aliceEncData)
    """)

    // `Id' cannot decrypt others Ids encrypted data
    illTyped("""
    (new Id).decrypt(aliceEncData)
    """)
  }
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
object DummyCloudTests {
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


object PullableCloud extends Id {
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
  def genChart[T <: Data with NotEncrypted](d: T): Chart = ???
}

object PullerAlice extends Id {
  import PullableCloud._

  def processData[T <: IdData](k: Key[T]): Chart =
    PullableCloud.read(k) match {
      case d: IdData with NotEncrypted =>
        PullableCloud.genChart(d)
      // case e: AESECS[IdData with NotEncrypted] =>
      //   PullableCloud.genChart(decrypt(e))
    }
}

object TestRunner extends App {
  EncryptionTests.run
}
