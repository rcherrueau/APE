// List in Scala
object list {
  trait List[+A] {
    def head: A
    def tail: List[A]

    def size: Int = this match {
      case Nil => 0
      case Cons(_, tl) => 1 + tl.size
    }

    def index(n: Int): A =
      if (n > size || n < 0) throw new IndexOutOfBoundsException(n.toString)
      else if (n > 0) tail.index(n - 1)
      else head
  }
  case class Cons[A](head: A,
                     tail: List[A]) extends List[A]
  case object Nil extends List[Nothing] {
    def head = throw new NoSuchElementException("VNil.head")
    def tail = throw new NoSuchElementException("VNil.tail")
  }

  object List {
    type Nil = Nil.type
  }

  // Tests:
  // import List._
  // Nil: List[Int]
  // Nil: Nil
  // Cons(1, Cons(2, Nil)): List[Int]
  // Cons(1, Cons(2, Nil)) .index (1)  // > 2
  // Cons(1, Cons(2, Nil)) .index (5)  // Type checks! Leads to an
  //                                   // Exception at runtime
  // Cons(1, Cons(2, Nil)) .index (-1) // Type checks! Leads to an
  //                                   // Exception at runtime
}

// Dependent types with Natural
object nat {
  trait Nat { def toInt: Int }
  case class Succ[P <: Nat](p: P) extends Nat { def toInt = p.toInt + 1 }
  case object _0 extends Nat { def toInt = 0 }

  object Nat {
    type _0 = _0.type
    type _1 = Succ[_0]
    type _2 = Succ[_1]
    type _3 = Succ[_2]
    type _4 = Succ[_3]
    type _5 = Succ[_4]
    // Macro ...

    val _1 = Succ(_0)
    val _2 = Succ(_1)
    val _3 = Succ(_2)
    val _4 = Succ(_3)
    val _5 = Succ(_4)
    // Macro ...
  }

  import Nat._
  import scala.annotation.implicitNotFound

  // Operations
  @implicitNotFound("Could not find the predecessor of ${N}")
  trait Pred[N <: Nat] {
    type Out <: Nat
    def out(n: N): Out
  }
  object Pred {
    def apply[N <: Nat](n: N)(implicit pred: Pred[N]) =
      pred.out(n)

    implicit def PredSuccN[N <: Nat] = new Pred[Succ[N]] {
      type Out = N
      def out(n: Succ[N]) = n.p
    }

    // Tests:
    // Pred.apply(_5)    : Succ[Succ[Succ[Succ[_0]]]]
    // Pred.apply(_5)    : _4
    // Pred(_5)          : Succ[Succ[Succ[Succ[_0]]]]
    // Pred(_5)          : _4
    // Pred.apply(_0)    // Doesn't type checksp
  }

  trait Sum[N1 <: Nat, N2 <: Nat] {
    type Out <: Nat
    def out(n1: N1, n2: N2): Out
  }
  object Sum {
    def apply[N1 <: Nat, N2 <: Nat](n1: N1,
                                    n2: N2)(
                                    implicit
                                    sum: Sum[N1, N2]) =
      sum.out(n1, n2)

    implicit def Sum_0N[N <: Nat] = new Sum[_0, N] {
      type Out = N
      def out(n1: _0, n2: N) = n2
    }

    // Tests:
    // Sum(_0, _4)          : _4
    // Sum(_0, _0)          : _0
    // Sum(_0, Pred(_1))    : _0
    // Sum(_0, Pred(_0))    // Doesn't type check

    implicit def SumSuccN1N2[N1 <: Nat,
                             N2 <: Nat](implicit
                                        sum: Sum[N1, N2]) =
      new Sum[Succ[N1], N2] {
        type Out = Succ[sum.Out]
        def out(n1: Succ[N1], n2: N2) = Succ(sum.out(n1.p, n2))
      }

    // Tests:
    // Sum(_5, _1)          : Succ[_5]
    // Sum(_5, Sum(_5, _1)) : Succ[Succ[Succ[Succ[Succ[Succ[_5]]]]]]
  }

  @implicitNotFound("${N1} minus ${N2} leads to a non Nat")
  trait Diff[N1 <: Nat, N2 <: Nat] {
    type Out <: Nat
    def out(n1: N1, n2: N2): Out
  }
  object Diff {
    def apply[N1 <: Nat, N2 <: Nat](n1: N1,
                                    n2: N2)(
                                    implicit
                                    diff: Diff[N1,N2]) =
      diff.out(n1, n2)

    implicit def DiffN_0[N <: Nat] = new Diff[N, _0] {
      type Out = N
      def out(n1: N, n2: _0) = n1
    }

    // Tests:
    // Diff(_4, _0)            : _4
    // Diff(_0, _0)            : _0
    // Diff(Sum(_1, _1), _0)   : _2

    implicit def DiffN1N2[N1 <: Nat,
                          N2 <: Nat](implicit
                                     diff: Diff[N1, N2]) =
      new Diff[Succ[N1], Succ[N2]] {
        type Out = diff.Out
        def out(n1: Succ[N1], n2: Succ[N2]) = diff.out(n1.p, n2.p)
      }

    // Tests:
    // Diff(_5, _1)   :_4
    // Diff(_0, _1)   // Doesn't type check
  }

  // Constraints:
  @implicitNotFound("${N1} is not lather than ${N2}")
  trait <[N1 <: Nat, N2 <: Nat]
  object < {
    implicit def lt_0SuccN[N <: Nat] = new <[_0, Succ[N]] {}
    implicit def ltN1N2[N1 <: Nat,
                        N2 <: Nat](implicit
                                   lt: N1 < N2) = new <[Succ[N1],Succ[N2]] {}

    // Tests:
    // implicitly [ _2 < _3 ]
    // implicitly [ _3 < _3 ] // No implicit found
    // implicitly [ _4 < _2 ] // No implicit found
  }

  @implicitNotFound("${N1} is not lather than or equal ${N2}")
  trait <=[N1 <: Nat, N2 <: Nat]
  object <= {
    implicit def lteq_0N[N <: Nat] = new <=[_0, N] {}
    implicit def lteqN1N2[N1 <: Nat,
                          N2 <: Nat](implicit
                                     lt: N1 <= N2) = new <=[Succ[N1],Succ[N2]] {}

    // Tests:
    // implicitly [ _2 <= _3 ]
    // implicitly [ _3 <= _3 ] // OK
    // implicitly [ _4 <= _2 ] // No implicit found
  }
}

// Use dependent types: Sized List
object sizedlist {
  import list._, List._
  import nat._, Nat._

  // private force creation of `SizedList` with the factory
  class SizedList[+L <: List[_], S <: Nat] private (val unsized: L) {
    def index[N <: Nat](n: N)(implicit
                              evLT: N < S) = unsized.index(n.toInt)
  }

  object SizedList {
    import scala.language.higherKinds

    type SNil = SizedList[Nil, _0]
    def SNil: SNil = new SizedList(Nil)

    type SCons[L<:List[_], S<:Nat] = SizedList[L,Succ[S]]
    def SCons[A, S <: Nat](a: A,
                           sl: SizedList[List[A],S]): SCons[List[A],S] =
      new SizedList(Cons(a, sl.unsized))
  }

  // Tests:
  // import SizedList._
  // SNil                     : SizedList[List[Int], _0]
  // SNil                     : SNil
  // SCons(1, SCons(2, SNil)) : SizedList[List[Int], _2]
  // SCons(1, SCons(2, SNil)) .index (_1)  // > 2
  // // Cons(1, Cons(2, Nil)) .index  (5)       // Type checks! Leads to an
  // //                                         // Exception at runtime
  // SCons(1, SCons(2, SNil)) .index (_5)       // Note: Doesn't type
  //                                            // check
  // // Cons(1, Cons(2, Nil)) .index (-1)       // Type checks! Leads to an
  // //                                         // Exception at runtime
  // SCons(1, SCons(2, SNil)) .index (Pred(_0)) // Note: Doesn't type
  //                                            // check
  // // Note: I can manually give a proof that `_2 < _5`
  // SCons(1, SCons(2, SNil)) .index (_5) { new <[_5,_2] {} } // Type
  //                                                          // checks!
  //                                                          // Leads to
  //                                                          // an
  //                                                          // Exception
  //                                                          // at
  //                                                          // runtime
}

// Use subtypeing: Generalization to Seq
object sized {
  // In the rest, we'ill use `scala.collection.List`
  // import list._, List._
  import nat._, Nat._

  // private force creation of `SizedList` with the factory
  class Sized[+CC <: Seq[_], S <: Nat] private (val unsized: CC) {
    def index[N <: Nat](n: N)(implicit
                              evLT: N < S) = unsized.drop(n.toInt).head
  }

  object Sized {
    import scala.language.higherKinds
    import scala.collection.generic.CanBuildFrom

    def SEmpty[CC[_] <: Seq[_]](implicit
                                cbf: CanBuildFrom[CC[Nothing],
                                                  Nothing,
                                                  CC[Nothing]]): Sized[CC[Nothing], _0] =
      new Sized(cbf().result)

    def SCons[A,
              CC[A] <: Seq[A],
              S <: Nat](a: A,
                        s: Sized[CC[A], S])(
                        implicit
                        cbf: CanBuildFrom[CC[A], A, CC[A]]): Sized[CC[A], Succ[S]] =
      new Sized({
                  val builder = cbf()
                  val seq = s.unsized

                  builder += a
                  seq foreach { builder += _ }
                  builder .result
                })
  }

  // Tests:
  // import Sized._
  // SEmpty[List]                       : Sized[List[Int], _0]
  // SEmpty[List]                       : Sized[List[Nothing], _0]
  // SEmpty[Seq]                        : Sized[Seq[Nothing], _0]
  // SCons(1, SCons(2, SEmpty[List]))   : Sized[List[Int], _2]
  // SCons(1, SCons(2, SEmpty[Seq]))    : Sized[Seq[Int], _2]
  // SCons(1, SCons(2, SEmpty[List])) .index (_1)      // > 2
  // SCons(1, SCons(2, SEmpty[Seq]))  .index (_1)      // > 2
  // // SCons(1, SCons(2, SEmpty[List])) .index (_5)      // Note: Doesn't
  // //                                                   // type check
  // // SCons(1, SCons(2, SEmpty[Seq])) .index (Pred(_0)) // Note: Doesn't
  // //                                                   // type check
}

object ClientApp extends App {
  // What we have with actual List
  {
    println("************************************************** List")
    import list._, List._

    Nil: List[Int]
    Nil: Nil
    Cons(1, Cons(2, Nil)): List[Int]

    println("value at index 1: " + Cons(1, Cons(2, Nil)) .index (1))

    // Both next expressions type check but lead to a runtime error:
    try {
      Cons(1, Cons(2, Nil)) .index (5)
    } catch {
      case r :RuntimeException => println("Type Checks but Runtime error")
    }

    try {
      Cons(1, Cons(2, Nil)) .index (-1)
    } catch {
      case r :RuntimeException => println("Type Checks but Runtime error")
    }
  }

  // What we want with List that uses dependent type
  {
    println("********************************************* SizedList")
    import list._, List._
    import nat._, Nat._
    import sizedlist._, SizedList._

    import utils.illTyped

    SNil                     : SizedList[List[Int], _0]
    SNil                     : SNil
    SCons(1, SCons(2, SNil)) : SizedList[List[Int], _2]

    illTyped("""
    SCons(1, SCons(2, SNil)) : SizedList[List[Int], _3]
    """)

    // Note: Here we use Nat instead of Int
    println("value at index 1: " + SCons(1, SCons(2, SNil)) .index (_1))

    illTyped("""
    SCons(1, SCons(2, SNil)) .index (_5)
    """)

    illTyped("""
    SCons(1, SCons(2, SNil)) .index (Pred(_0))
    """)
  }

  // Generalization to all Seq
  {
    println("************************************************* Sized")
    import nat._, Nat._
    import sized._, Sized._

    import utils.illTyped

    // Working with List
    SEmpty[List]                     : Sized[List[Int], _0]
    SCons(1, SCons(2, SEmpty[List])) : Sized[List[Int], _2]

    illTyped("""
    SCons(1, SCons(2, SEmpty[List])) : Sized[List[Int], _3]
    """)

    println("value at index 1: " + SCons(1, SCons(2, SEmpty[List])) .index (_1))

    illTyped("""
    SCons(1, SCons(2, SEmpty[List])) .index (_5)
    """)

    illTyped("""
    SCons(1, SCons(2, SEmpty[List])) .index (Pred(_0))
    """)

    // Working with Stream
    SEmpty[Stream]                     : Sized[Stream[Int], _0]
    SCons(1, SCons(2, SEmpty[Stream])) : Sized[Stream[Int], _2]

    illTyped("""
    SCons(1, SCons(2, SEmpty[Stream])) : Sized[Stream[Int], _3]
    """)

    println("value at index 1: " + SCons(1, SCons(2, SEmpty[Stream])) .index (_1))

    illTyped("""
    SCons(1, SCons(2, SEmpty[Stream])) .index (_5)
    """)

    illTyped("""
    SCons(1, SCons(2, SEmpty[Stream])) .index (Pred(_0))
    """)
  }
}

// Le mot de la fin: http://stackoverflow.com/a/12937819/2072144
