// http://danielwestheide.com/blog/2013/02/13/the-neophytes-guide-to-scala-part-13-path-dependent-types.html
// http://stackoverflow.com/a/12937819
// https://github.com/milessabin/shapeless

// With, dependate data I have to find a new way to do the read and
// store.

import shapeless.{Data => ShapelessData, _}

sealed abstract class Data

trait Id {
  sealed abstract class IdData extends Data
  case class Raw() extends IdData
  case class Encrypted[+T <: Data](data: T) extends IdData
  case class Pull() extends IdData
  case class Chart() extends IdData

  def genData: IdData = Raw()
  def encrypt[T <: Data](d: T): Encrypted[T] = Encrypted(d)
  def decrypt[T <: Data](d: Encrypted[T]): T = d.data
}

case object Alice extends Id
case object Bob extends Id

object Cloud extends Id {
  import collection.mutable.Map
  import scala.util.Random
  import annotation.implicitNotFound

  // T is the type of the corresponding Data
  class Key[T] {
    val id = Random.nextLong()
  }

  val database = Map.empty[Any, Any]

  def store[T <: Data](d: T): Key[T] = {
    val key = new Key[T]()

    database.update(key, d)
    key
  }

  def read[T](k: Key[T]): Option[T] = null

  // database.get(k) match {
  //   case Some(d) => d match {
  //     case r: Raw => r
  //     case e: Encrypted[_] => e.data
  //     case p: Pull => p
  //   }
  //   case None => o.Raw()
  // }

  def genChart[T](o: Id)(d: T)(implicit T: Is[T]): o.Chart = o.Chart()
  @implicitNotFound("Type mismatch ${T} is forbidden")
  case class Is[+T]()
  implicit object AliceIsRaw extends Is[Alice.Raw]

  // def getCals[T <: Data](o: Id)(k: Key[T]): o.Chart = {
  //   val data = read(k).get

  //   genChart(o)(data)
  // }
}

object Test {
  // Everithing is raw data: The program type checks
  def scenario1 {
    val data = Alice.genData
    val key = Cloud.store(data)

    val readedData = Cloud.read(key)
    Cloud.genChart(Alice)(readedData.get)
  }

  // // The Cloud.genChart doesn't accept encrypted data: The program
  // // doesn't type check arround getCals1
  // def scenario2 {
  //   val data = Alice.encrypt(Alice.genData)
  //   val key = Cloud.store(data)

  //   val readedData = Cloud.read(key).get
  //   Cloud.genChart(Alice)(readedData)
  // }

  // // In Cloud.getCals2 Alice tries to decrypt Bob encrypted data: The
  // // program doesn't type check.
  // def scenario3 {
  //   val data = Bob.encrypt(Bob.genData)
  //   val key = Cloud.store(data)

  //   val readedData = Cloud.read(key).get
  //   val decryptedData = Alice.decrypt(readedData)
  //   Cloud.genChart(Alice)(decryptedData)
  // }

  // In Cloud.getCals2 Alice decrypts her data before calling
  // Cloud.genChart: The program type checks.
  def scenario4 {
    val data = Alice.encrypt(Alice.genData)
    val key = Cloud.store(data)

    val readedData = Cloud.read(key).get
    val decryptedData = Alice.decrypt(readedData)
    Cloud.genChart(Alice)(decryptedData)
  }

  def main(args: Array[String]) {

  }
}
