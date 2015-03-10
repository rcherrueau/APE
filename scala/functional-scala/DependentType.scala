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
  // Nat is a trait indicating a naturel number, and Succ is a subtype
  // of it that takes a Nat as a type parameter.
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

  // Operations:

  // We don't define operation for our Nats as methods on our types,
  // but rather as types themselves. Below is the definition of the
  // dependent type `Pred`, which takes a Nat and returns it's
  // predecessor as that number decremented by one.

  // Pred is a Type class that witnesses that `Out` is the predecessor
  // of `N`. It's used with the style `implicitly[ Pred[_2] ]` which
  // returns a Pred trait with an `Out` type of `_1` and with a method
  // `out` that takes a `_2` value and return a `_1`.. But how does it
  // go from a definition that takes one generic parameter to
  // determine the predecessor of our number? Look at the `Pred.apply`
  // to find the answer.
  @implicitNotFound("Could not find the predecessor of ${N}")
  trait Pred[N <: Nat] {
    type Out <: Nat
    def out(n: N): Out
  }
  object Pred {
    // `apply` is a method that takes a type class `Pred` on `N`. When
    // we call `apply`, scala compiler search for an implicit that has
    // the same type as the type passed into implicit. For instance,
    // if we call `apply` with value `_2`, the compiler will use the
    // implicit definition `PredSuccN` to instantiate our type class.
    // And if we try `apply(_0)` the compiler will complain about not
    // being able to find an implicit value for the parameter `pred:
    // Pred[_0]` because there is no implicit definition with a
    // signature `Pred[N]` or `Pred[_0]`.
    def apply[N <: Nat](n: N)(implicit pred: Pred[N]): pred.Out =
      pred.out(n)

    // Implements the behaviour of Pred. The implicit method
    // instantiate a `Pred` for a given Nat.
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
  // Thus, Pred is a Π-type (Dependent product type). It's a function
  // from a value `n` to type `Out`. In other word, pred takes a type
  // `N =:= Succ[_ <: Nat]` and return a type `Out` that is type `N`
  // predecessor.


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

  // `a * b` is equivalent to `a + a + a + ... + a` b times. Thus in
  // `Prod` we wanna reuse `Sum`
  trait Prod[N1 <: Nat, N2 <: Nat] {
    type Out <: Nat
    def out(n1: N1, n2: N2): Out
  }
  // XXX:
  // object Prod {
  //   def apply[N1 <: Nat, N2 <: Nat](n1: N1,
  //                                   n2: N2)(
  //                                   implicit
  //                                   prod: Prod[N1, N2]) =
  //     prod.out(n1, n2)
  //
  //   implicit def ProdN_1[N <: Nat] = new Prod[N, _1] {
  //     type Out = N
  //     def out(n1: N, n2: _1) = n1
  //   }
  //   implicit def ProdN1SuccN2[N1 <: Nat,
  //                             N2 <: Nat](implicit
  //                                        prod: Prod[N1, N2],
  //                                        // Note: illegal dependent
  //                                        // method type: parameter
  //                                        // appears in the type of
  //                                        // another parameter in the
  //                                        // same section or an earlier
  //                                        // one
  //                                        sum: Sum[N1, prod.Out]) =
  //     new Prod[N1, Succ[N2]] {
  //       type Out = Sum[N1, prod.Out]
  //       // Note: Sum(...) requires a implicit Sum[N1, prod.Out]
  //       def out(n1: N1, n2: Succ[N2]) = Sum(n1, prod.out(n1, n2.p))
  //     }
  // }
  // Solution:
  object Prod {
    def apply[N1 <: Nat, N2 <: Nat](n1: N1,
                                    n2: N2)(
                                    implicit
                                    prod: Prod[N1, N2]) =
      prod.out(n1, n2)

    // N1 * N2 = R
    type Aux[N1 <: Nat,
             N2 <: Nat,
             R <: Nat] = Prod[N1, N2] { type Out = R }

    implicit def ProdN_0[N1 <: Nat]: Aux[N1, _0, _0] = new Prod[N1, _0] {
      type Out = _0
      def out(n1: N1, n2: _0) = _0
    }

    // Scala doesn't like you!
    implicit def ProdN1SuccN2[N1 <: Nat,
                              N2 <: Nat,
                              R  <: Nat](
                              implicit
                              prod: Prod.Aux[N1, N2, R],
                              sum: Sum[N1, R]): Aux[N1, Succ[N2], sum.Out] =
      new Prod[N1, Succ[N2]] {
        type Out = sum.Out
        def out(n1: N1, n2: Succ[N2]) = sum.out(n1, prod.out(n1, n2.p))
      }

    // Tests:
    // Prod(_5, _1)  : _5
    // Prod(_5, _2)  : Succ[Succ[Succ[Succ[Succ[_5]]]]]
    // Prod(_2, _2)  : _4
    // Prod(_5, _0)  : _0
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
  //*
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
  // */

  //*
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
  // */

  //*
  // Generalization to all Seq
  {
    println("********************************************** Sized[_]")
    import nat._, Nat._
    import sized._, Sized._

    import utils.illTyped

    // Note: Working with List
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

    // Note: Working with Stream
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
  // */
}

// Resources:
// @InProceedings{OMO10a,
//   author =    {Bruno C. d. S. Oliveira and Adriaan Moors and Martin
//                Odersky},
//   title =     {Type classes as objects and implicits},
//   booktitle = {Proceedings of the 25th Annual {ACM} {SIGPLAN}
//                Conference on Object-Oriented Programming, Systems,
//                Languages, and Applications, {OOPSLA} 2010, October
//                17-21, 2010, Reno/Tahoe, Nevada, {USA}},
//   pages =     {341--360},
//   year =      2010,
//   url =       {http://doi.acm.org/10.1145/1869459.1869489},
//   doi =       {10.1145/1869459.1869489},
//   timestamp = {Wed, 27 Oct 2010 13:53:08 +0200},
//   biburl =
//                {http://dblp.uni-trier.de/rec/bib/conf/oopsla/OliveiraMO10},
//   bibsource = {dblp computer science bibliography, http://dblp.org}
// }
//
// https://markehammons.wordpress.com/2013/07/02/dependent-types-in-scala/
// https://github.com/milessabin/shapeless/

// Le mot de la fin: http://stackoverflow.com/a/12937819/2072144
