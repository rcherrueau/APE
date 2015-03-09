// http://wheaties.github.io/Presentations/Scala-Dep-Types/dependent-types.html#/

object svect1 {
  import shapeless.{Nat, Succ, ops}, ops.nat._, Nat._

  // Vector
  trait Vect[+A] {
    def head: A
    def tail: Vect[A]
    def size: Int = this match {
      case VNil => 0
      case VCons(_, tl) => 1 + tl.size
    }
    def index(n: Int): A =
      if (n > size) throw new IndexOutOfBoundsException()
      else if (n > 0) tail.index(n - 1)
      else head
  }

  case class VCons[A, T <: Vect[A]](head: A, tail: T) extends Vect[A]

  case object VNil extends Vect[Nothing] {
    def head = throw new NoSuchElementException("VNil.head")
    def tail = throw new NoSuchElementException("VNil.tail")
  }

  object Vect {
    type VNil = VNil.type
  }
  import Vect._

  // Usage
  val a: Vect[Int] = VCons(1, VCons(2, VCons(3, VNil)))
  a.head: Int
  // a.tail.tail.tail.tail.tail.tail.tail.tail.tail // Type Checks!
  //                                                // Exception at
  //                                                // runtime
  // a.index(1000000) // Type Checks!
  //                  // Exception at runtime


  // Compute the size of an Vector. To compute the size, I have to
  // print in the type, the internal structure of that Vect. Thus,
  // VCons is parametrized by its Tail. This doesn't make sense, it's
  // redundant information. A solution is that a sized is based on an
  // empty vect. Thus we don't have to compute the size of the vector
  // and we don't need `Sizer` type class.
  trait Sizer[V <: Vect[_]] {
    type Size <: Nat
    def size(v: V): Size
  }

  object Sizer {
    def apply[V <: Vect[_]](v: V)(implicit sz: Sizer[V]) = sz.size(v)

    implicit def SizerVNil = new Sizer[VNil] {
      type Size = _0
      def size(v: VNil) = _0
    }
    val a: _0 = Sizer(VNil)

    implicit def SizerVCons[A, T <: Vect[A]](implicit sz: Sizer[T]) = new Sizer[VCons[A,T]] {
      type Size = Succ[sz.Size]
      def size(v: VCons[A,T]) = Succ[sz.Size]()
    }
    val b: _2 = Sizer(VCons(1, VCons(1, VNil)))

  }

  // Sized: Wrapper witnessing that it has the statically specified
  // size.

  class SizedVect[V <: Vect[_], S <: Nat](val unsized: V)
  object SizedVect {
    def apply[V<:Vect[_]](v: V)(implicit sz: Sizer[V]): SizedVect[V, sz.Size] = new SizedVect(v)
  }

  case class SizedOps[V <: Vect[_], S <: Nat](s: SizedVect[V, S]) { slef =>

    def index(n: Nat)(implicit
                      lt: LTEq[n.N, S],
                      ev: ToInt[n.N]) = s.unsized.index(toInt[n.N])

  }

  SizedOps(SizedVect(VCons(1, VCons(2, VNil)))).index(_2)
}


object svect2 extends App {
  import shapeless.{Nat, Succ, ops}, ops.nat._, Nat._

  trait Vect[+A] {
    def head: A
    def tail: Vect[A]
    def size: Int = this match {
      case VNil => 0
      case VCons(_, tl) => 1 + tl.size
    }
    def index(n: Int): A =
      if (n > size) throw new IndexOutOfBoundsException()
      else if (n > 0) tail.index(n - 1)
      else head
  }
  // Here VCons is no more parameteryzed by it's tail
  case class VCons[A](head: A, tail: Vect[A]) extends Vect[A]
  case object VNil extends Vect[Nothing] {
    def head = throw new NoSuchElementException("VNil.head")
    def tail = throw new NoSuchElementException("VNil.tail")
  }

  object Vect {
    type VNil = VNil.type
  }
  import Vect._

  class SizedVect[+V <: Vect[_], S <: Nat](val unsized: V) {
    def index(n: Nat)(implicit
                      ev1: LT[n.N, S],
                      ev2: ToInt[n.N]) = unsized.index(toInt[n.N])
  }

  object SizedVect {
    val SVNil: SizedVect[VNil, _0] = new SizedVect(VNil)
    type SVNil = SVNil.type

    def  SVCons[A,
                V <: Vect[A],
                S <: Nat](a: A, sv: SizedVect[V,S]): SizedVect[VCons[A], Succ[S]] =
      new SizedVect(VCons(a, sv.unsized))
    type SVCons[A,S <: Nat] = SizedVect[VCons[A], S]
    type SVect[A,S<:Nat] = SizedVect[Vect[A],S]
  }

  import SizedVect._

  val a = SVNil: SVNil
  val b = SVNil: SVect[Nothing, _0] // Required SizedVect to be covariant on V
  val c = SVCons(1, SVCons(2, SVNil)): SVCons[Int, _2]
  val d = SVCons(1, SVCons(2, SVNil)): SVect[Int, _2]
  // SVCons(1, SVCons(2, SVNil)): SVect[Int, _3] // Doesn't typecheck

  // println(a.index(_0)) // Doesn't type check
  println(c.index(_0))
  println(c.index(_1))
  // println(c.index(_2)) // Doesn't type check
  // I can send a proof by hand:
  println(c.index(_2)(new LT[_2,_2] {},implicitly)) // leads to a
                                                    // runtime error

  // Generalization to Seq
  class Sized[+CC <: Seq[_], S <: Nat](val unsized: CC) {
    // // Error in the implementation
    // def index(n: Nat)(implicit
    //                   ev1: LTEq[n.N, S],
    //                   ev2: ToInt[n.N]) = unsized.index(toInt[n.N])
    def index(n: Nat)(implicit
                      ev1: LT[n.N, S],
                      ev2: ToInt[n.N]) = unsized.drop(toInt[n.N]).head
  }

  object Sized {
    import scala.language.higherKinds
    import scala.collection.generic.CanBuildFrom

    def SEmpty[CC[_] <: Seq[_]](implicit
                               cbf: CanBuildFrom[CC[Nothing], Nothing, CC[Nothing]]) : Sized[CC[Nothing], _0] =
      new Sized(cbf().result())

    def SCons[A,
              CC[A] <: Seq[A],
              S <: Nat](a: A,
                        s: Sized[CC[A], S])(
                        implicit
                        cbf: CanBuildFrom[CC[A], A, CC[A]]):
        Sized[CC[A], Succ[S]] = new Sized({
                                            val builder = cbf()
                                            builder += a
                                            s.unsized.foreach { builder += (_:A) }
                                            builder.result()
                                          })
  }
  import Sized._


  val seq: Sized[Seq[Nothing], _0] = SEmpty[Seq]
  val list: Sized[List[Nothing], _0] = SEmpty[List]

  val l2: Sized[List[Int], _1] = SCons(1, SEmpty[List])
  val l3: Sized[List[Int], _2] = SCons(1, SCons(1, SEmpty[List]))

  val l4 = SCons(1, SCons(1, SEmpty[List]))
}
