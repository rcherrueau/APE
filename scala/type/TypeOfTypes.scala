/** Scala's Type of Types
  *
  * A full list of all trick what we can to with Types in Scala. Real
  * life examples why we'd need these types.
  *
  * [[http://ktoso.github.io/scala-types-of-types/]]
  */

/** 4. Unified Type System -- Any, AnyRef, AnyVal
  *
  * {{{
  * scala> :load TypeOfTypes.scala
  * scala> :javap Test4
  * }}}
  */
object Test4 {
  import scala.collection.mutable.ArrayBuffer

  class Person

  val allThings = ArrayBuffer[Any]()
  val myInt = 42

  allThings += myInt
  allThings += new Person()
  // 35: invokevirtual #47                 // Method myInt:()I
  // 38: invokestatic  #53                 // Method scala/runtime/BoxesRunTime.boxToInteger:(I)Ljava/lang/Integer;
  // 41: invokevirtual #57                 // Method scala/collection/mutable/ArrayBuffer.$plus$eq:(Ljava/lang/Object;)Lscala/collection/mutable/ArrayBuffer;
  //
  // You’ll notice that myInt is still carrying the value of a int
  // primitive (this is visible as `I' at the end of the `myInt:()I'
  // invokevirtual call). Then, right before adding it to the
  // ArrayBuffer (41), scalac inserted a call to
  // BoxesRunTime.boxToInteger:(I)Ljava/lang/Integer (a small hint for
  // not frequent bytecode readers, the method it calls is: public
  // Integer boxToInteger(i: int)) (38). This way, by having a smart
  // compiler and treating everything as an object in this common
  // hierarchy we’re able to get away from the "but primitives are
  // different" edge-cases, at least at the level of our Scala source
  // code.
}

/** 5. The Bottom Types -- Nothing and Null */
object Test5 {
  // `thing' is type Int but how type inferencer can still work, and
  // infer sound types when working with "weird" situation like
  // throwing exceptions?
  val thing: Int =
    if (false)
      42
    else
      throw new Exception("Whoops")
  // The previous expression types checks! The inferencer was able to
  // infer that the `thing' value will only ever be of type `Int'.
  // This is because fo the Bottom Type property of `Nothing'. Type
  // inference always looks for the "common type" of both branches in
  // an if statement and `Nothing' extends everything (i.e.: every
  // types has `Nothing' as a subtype). Thus `Nothing' is a subtype of
  // `Int' and the common type of both branches is `Int'.

  // The same reasoning can be applied to the second Bottom Type in
  // Scala: `Null'
  val otherThing: String =
    if (false)
      "Yay!"
    else
      null
  // Just keep in mind the `Nothing' extends anything (i.e.: `Any')
  // whereas `Null' extends `AnyVal'. To ensure that, use `:type'
  // command (which allows to get the type of an expression) in the
  // REPL:
  //
  // > :type -v throw new Exception()
  // // Type signature
  // Nothing
  //
  // // Internal Type structure
  // TypeRef(TypeSymbol(final abstract class Nothing extends Any))
  //
  // > :type -v null
  // // Type signature
  // Null
  //
  // // Internal Type structure
  // TypeRef(TypeSymbol(final abstract class Null extends AnyRef))
}

/** 7. Type Variance in Scala
  *
  * Variance can be explained as "type compatible-ness" between types:
  * {{{
  * Name            Description     Scala syntax
  * --------------+---------------+--------------
  * Invariant       C[T'] ≠ C[T]    C[T]
  * Covariant       C[T'] <: C[T]   C[+T]
  * Contravariant   C[T'] :> C[T]   C[-T]
  * }}}
  *
  * In general, immutable collections are covariant and mutaboe
  * collections are invariant.
  */
object Test7 {
  class Fruit
  case class Apple() extends Fruit
  case class Orange() extends Fruit

  // In the first example we use List[+A].
  val l1: List[Apple] = Apple() :: Nil
  val l2: List[Fruit] = Orange() :: l1
  val l3: List[AnyRef] = "" :: l2
  // Having immutable collections co-variant is safe. The same connot
  // be said about mutable collections. The classic example is
  // Array[T] wich is invariant. This a common problem that Java
  // solves using dynammic checking.
  // val a: Array[Any] = Array[Int](1,2,3) // won't type check
}

/** 8. Refined Types (refinements)
  *
  * Subclassing without naming the subclass.
  */
object Test8 {
  class Entity {
    def persistForReal() = {}
  }

  trait Persister {
    def doPersist(e: Entity) = {
      e.persistForReal()
    }
  }

  // Our redifined instance (and type):
  val redefinedMockPersister = new Persister {
    override def doPersist(e: Entity) = ()
  }
}

/** 9. Package Object */
//
// Package Object provide a useful pattern for "importing a bunch of
// stuff together" as well as being one of the places to look for
// implicit values.
//
// It’s a custom to put package object’s in a file called
// package.scala into the package they’re the object for.
//
// src/main/scala/com/garden/apples/package.scala
//
// package com.garden
//
// package object apples extends RedApplest with GreenApples {
//   val redApples = List(red1, red2)
//   val greenApples = List(green1, green2)
// }
//
// trait RedApples {
//   val red1, red2 = "red"
// }
//
// trait GreenApples {
//   val green1, green2 = "green"
// }
//
// On the usage side, when you import "the package", you import any
// state that is defined in the package side.
//
// import com.garden.apples._
// redApples foreach println

/** 10. Types Alias */
object Test10 {
  // It's a trick we can use to make our code more readable.
  type User = String
  type Age = Int

  // Name our primitives with aliases to write a Map definition that
  // "makes sense!"
  val data: Map[User, Age] = Map.empty
}

/** 11. Abstract Type Member */
object Test11 {
  // For Java folks, following may seem very similar to the
  // `SimplestContainer<A>' syntax. But, it's a bit more powerful.
  trait SimplestContainer {
    type A // Stands for `type A >: Nothing <: Any'
    def value: A
  }

  // Because type is abstract, Now we can constraint our container on
  // types. For example we can have a container that can only store
  // `Number' (from java.lang).
  //
  // /!\ Here we use constraint `>:' to say "any A wich is reductible
  // to a Number". This makes sens because scala offers an implicit
  // library which convert any number like into a `Number'.
  trait OnlyNumbersContainer extends SimplestContainer {
    type A >: Number
  }
  val ints1 = new OnlyNumbersContainer {
    def value = 12
  }


  // Or we can add constraint later in the class hierarchy, for
  // instance by mixing in a trait that states "only Numbers".
  //
  // /!\ Here we use constraint `>:' to say "any A wich is reductible
  // to a Number". This makes sens because scala offers an implicit
  // library which convert any number like into a `Number'.
  trait OnlyNumbers {
    type A >: Number
  }
  val ints2 = new SimplestContainer with OnlyNumbers {
    def value = 12
  }
  // The following won't type check because there is no implicit rules
  // which transform an `String' into a `Number'. If we want that this
  // expression type checks, we have to introduce a new implicit rule.
  // val strs = new SimplestContainer with OnlyNumbers {
  //   def value = ""
  // }
}

/** 12. F-Bounded Type
  *
  * It enables a sort of "self-referential" type constraint using to
  * solve how to define a polymorphic function that when it's called
  * at a subtype level, the polymorphic function *retains* the type of
  * the subtype.
  */
object Test12 {
  /** F-Bounded for parameters
    *
    * Use F-Bounded Type to add constraint on a polymorphic function.
    * In this case the polymorphic function only accepts as parameter
    * subtypes of the concrete instance.
    */
  object Test1 {
    trait Fruit[T <: Fruit[T]] {
      final def compareTo(other: T): Boolean =
        // implem doesn't matter in our example but the method should
        // be final for the correctness.
        true
    }

    class Apple extends Fruit[Apple]
    class Orange extends Fruit[Orange]

    val apple = new Apple
    val orange = new Orange

    // apple compareTo orange // won't type check
                              // > found: Orange, required: Apple
  }

  /** F-Bounded for return type
    *
    * Use F-Bounded Type to add constraint on a polymorphic function.
    * In this case the polymorphic function, when passed a value of
    * some subtype will always return a value of the same subtype as
    * its parameter.
    *
    * [[http://logji.blogspot.se/2012/11/f-bounded-type-polymorphism-give-up-now.html]]
    */
  object Test2 {
    trait Account[T <: Account[T]] {
      // Always returns a value of the same subtype as its concrete
      // instance.
      def addFunds(amount: Double): T
    }

    class BrokerageAccount(total: Double) extends Account[BrokerageAccount] {
      override def addFunds(amount: Double) =
        new BrokerageAccount(total + amount)
    }

    class SavingsAccount(total: Double) extends Account[SavingsAccount] {
      override def addFunds(amount: Double) =
        new SavingsAccount(total + amount)
    }

    object Account {
      // Always returns a value of the same subtype as its parameter.
      // When you pass a `SavingsAccount', the methods returns
      // something with the type `SavingAccount'. It works with one
      // account at a time.
      def deposit[T <: Account[T]](amount: Double, account: T): T =
        account.addFunds(amount)

      // Pending Questions:
      //
      // 1. How to properly refer to the abstract supertype?
      def debpositAll(amount: Double,
                      // We *existentially bounded the type of each
                      // member* of the List with the `forSome'
                      // keyword instead of *existentially bounded the
                      // type of List* as a whole.
                      accounts: List[T forSome { type T <: Account[T] }]):
                      List[T forSome { type T <: Account[T] }] =
        accounts map { deposit(amount, _) }
    }

    // 2. Am I sure that the type bound is "self-referential"? Nop!
    // The following type checks because F-Bounded parameter states
    // that a subtype must be parameterized by *some potentially
    // other subtype*.
    class MalignantAccount(total: Double) extends Account[SavingsAccount] {
      override def addFunds(amount: Double) =
        new SavingsAccount(total + amount)
    }
    // Fortunately, calling deposit on it won't type checks:
    // Account.deposit(100, new MalignantAccount(100))
  }
}
