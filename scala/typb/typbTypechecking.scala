// http://danielwestheide.com/blog/2013/02/13/the-neophytes-guide-to-scala-part-13-path-dependent-types.html
// http://stackoverflow.com/a/12937819
// https://github.com/milessabin/shapeless

// sealed abstract class Id
// sealed abstract class Data
// case class RawData extends Data
// case class EncryptedData extends Data


// object CloudData {
//   abstract class Key { type DataType; val name: String}
// }

// import CloudData.Key

// class CloudData {
//   import collection.mutable.Map
//   val data = Map.empty[Key, Any]
//   def query(key: Key): key.DataType = data.get(key).asInstanceOf[key.DataType]
//   def store(key: Key)(value: key.DataType): Unit = data.update(key, value)
// }

// case class Raw(name: String) extends Key {
//   type DataType = RawData
// }
// case class Encrypted(name: String) extends Key {
//   type DataType = EncryptedData
// }
import scala.util.Random


// abstract class Data
// case class RawData extends Data
// case class EncryptedData extends Data

// abstract class Key(name: Long) {
//   type Nature
// }

// object Cloud {
//   import collection.mutable.Map

//   val database = Map.empty[Key, Any]

//   def store(data: Data)(key: Key): Key = {
//     // val key = new Key(Random.nextLong()) { type Nature = data.type }
//     database.put(key, data)
//     key
//   }

//   def read(key: Key): Option[key.Nature] =
//     database.get(key).asInstanceOf[Option[key.Nature]]

//   // def doSomeStuffWithEncrypted(data: Data[Encrypted]) =
//   //   println("toSomeStuffWithEncrypted")

//   // def doSomeStuffWithRaw(data: Data[Raw]) =
//   //   println("toSomeStuffWithRaw")
// }



object Cloud {
  abstract class Key(id: Long) { type Nature }

  import collection.mutable.Map

  val database = Map.empty[Key, Any]

  def read(key: Key): Option[key.Nature] =
    database.get(key).asInstanceOf[Option[key.Nature]]

  def store(key: Key)(data: Nature): Unit =
    database.update(key, data)
}

sealed abstract class Nature
case class Raw extends Nature
case class Encrypted extends Nature

object TYPB_TypeChecking {
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

  def main(args: Array[String]) {
    import Cloud.Key

    val rawKey = new Key(10000) { type Nature = Raw }
    Cloud.store(rawKey)(Raw())
    val rawData: Option[Raw] = Cloud.read(rawKey)

    val encKey = new Key(10000) { type Nature = Encrypted }
    Cloud.store(encKey)(Encrypted())
    val encData: Option[Encrypted] = Cloud.read(encKey)

    // println(key.asInstanceOf[AnyRef].getClass.getSimpleName)
    // val cloudData = new CloudData
    // cloudData.store(Raw("Alice"))(RawData())
    // cloudData.store(Raw("Bob"))(Encrypted())
  }
}

// Constraint on Data
// sealed abstract class Label
// case class Raw extends Label
// case class Encrypted extends Label

// case object Alice extends Id {
//   def getPrivate: Data[Alice.type] = null
//   def encrypt[U <: Id](data: Data[U]): Encrypt[Alice.type, U] = null
//   def decrypt[U <: Id](data: Encrypt[Alice.type, U]): Data[U] = null
// }

// case object Bob extends Id {
//   def getPrivate: Data[Bob.type] = null
//   def encrypt[U <: Id](data: Data[U]): Encrypt[Bob.type, U] = null
//   def decrypt[U <: Id](data: Encrypt[Bob.type, U]): Data[U] = null
// }

// case object Cloud extends Id {
//   def store[U <: Id](data: Data[U]): Unit = {}
//   def query[U <: Id](a: U): Data[U] = null
//   def genChart[U <: Id](data: Data[U]): Unit = {}
//   def getCals1[U <: Id](a: U): Unit = {
//     val data = query(a)
//     genChart(data)
//   }

//   // def getCals2[U <: Id](a: U): Unit = {
//   //   val data = query(a)
//   //   val data2 = a.decrypt(data)
//   //   genChart(data)
//   // }

// }

// Data type: Owner, Label
// sealed abstract class Data[T <: Id, U <: Label]
// case class Text[T <: Id] extends Data[T, Raw]
// case class Encrypt[T <: Id, U <: Id, V <: Label](data: Data[U,V]) extends Data[T, Encrypted]

// case object Alice extends Id {
//   def encrypt[T <: Id, U <: Label](data: Data[T,U]): Encrypt[Alice.type, T, U] = null
//   def decrypt(data: Data[Alice.type, Encrypted])
// }


// sealed abstract case class Data[T <: Id]


// sealed abstract class Data[T <: Id]
// // T: Encrypter, U: EncryptedDataOwner
// case class Encrypted[T <: Id, U <: Id](data: Data[U]) extends Data[T]
