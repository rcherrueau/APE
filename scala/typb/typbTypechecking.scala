sealed abstract class Id

case object Alice extends Id {
  def getPrivate: Data[Alice.type] = null
  def encrypt[U <: Id](data: Data[U]): Encrypt[Alice.type, U] = null
  def decrypt[U <: Id](data: Encrypt[Alice.type, U]): Data[U] = null
}

case object Bob extends Id {
  def getPrivate: Data[Bob.type] = null
  def encrypt[U <: Id](data: Data[U]): Encrypt[Bob.type, U] = null
  def decrypt[U <: Id](data: Encrypt[Bob.type, U]): Data[U] = null
}

case object Cloud extends Id {
  def store[U <: Id](data: Data[U]): Unit = {}
  def query[U <: Id](a: U): Data[U] = null
  def genChart[U <: Id](data: Data[U]): Unit = {}
  def getCals[U <: Id](a: U): Unit = {
    val data = query(a)
    genChart(data)
  }
}

sealed abstract class Data[T <: Id]
case class Encrypt[U <: Id, T <: Id](data: Data[T]) extends Data[U]

object TYPB_TypeChecking {

  def scenario1 {
    val data = Alice getPrivate ;
    Cloud store data
    Cloud getCals Alice
  }

  def scenario2 {
    val data = Alice encrypt (Alice getPrivate)
    Cloud store data
    Cloud getCals Alice
  }

  def scenario3 {
    val data = Alice encrypt (Alice getPrivate)
    Cloud store data
    Cloud getCals Alice
  }

  def main(args: Array[String]) {

  }
}
