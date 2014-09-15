// http://danielwestheide.com/blog/2013/02/13/the-neophytes-guide-to-scala-part-13-path-dependent-types.html
// http://stackoverflow.com/a/12937819
// https://github.com/milessabin/shapeless

// The program enventualy type checks! Next step is in store, try make
// the key implicit: there is no way to do that!

object Cloud {
  import collection.mutable.Map
  object KeyMaker {
    import scala.util.Random
    object Raw {
      def apply() = new Key(Random.nextLong()) { type Nature = Raw }
    }
    object Encrypted {
      def apply() = new Key(Random.nextLong()) { type Nature = Encrypted }
    }
  }

  abstract class Key(id: Long) { type Nature }

  val database = Map.empty[Key, Any]

  def read(key: Key): Option[key.Nature] =
    database.get(key).asInstanceOf[Option[key.Nature]]

  def store(data: Nature)(key: Key): Unit =
    database.update(key, data)
}

sealed abstract class Nature
case class Raw extends Nature
case class Encrypted extends Nature

object TYPB_TypeChecking {
  def main(args: Array[String]) {
    import Cloud.KeyMaker

    val rawKey = KeyMaker.Raw()
    Cloud.store(Raw())(rawKey)
    val rawData: Option[Raw] = Cloud.read(rawKey)

    val encKey1 = KeyMaker.Encrypted()
    Cloud.store(Encrypted())(encKey1)
    val encData1: Option[Encrypted] = Cloud.read(encKey1)

    val encKey2 = KeyMaker.Encrypted()
    Cloud.store(Encrypted())(encKey2)
    val encData2: Option[Encrypted] = Cloud.read(encKey2)
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
