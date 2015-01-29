/** Type Classes as Object and Implicits
  *
  * @InProceedings{OMO10a,
  *   author =       {Bruno C. d. S. Oliveira and Adriaan Moors and Martin
  *                   Odersky},
  *   title =        {Type classes as objects and implicits},
  *   booktitle =    {Proceedings of the 25th Annual {ACM} {SIGPLAN}
  *                   Conference on Object-Oriented Programming, Systems,
  *                   Languages, and Applications, {OOPSLA} 2010, October
  *                   17-21, 2010, Reno/Tahoe, Nevada, {USA}},
  *   pages =        {341--360},
  *   year =         2010,
  *   url =          {http://doi.acm.org/10.1145/1869459.1869489},
  *   doi =          {10.1145/1869459.1869489},
  *   timestamp =    {Wed, 27 Oct 2010 13:53:08 +0200},
  *   biburl =
  *                   {http://dblp.uni-trier.de/rec/bib/conf/oopsla/OliveiraMO10},
  *   bibsource =    {dblp computer science bibliography, http://dblp.org}
  * }
  */
object OMO10a {

  // First, implicit prevent the cumbersome of passing explicit
  // constraint to generic algorithms. This is especially true when
  // many generic algorithmes require multiple constraints on their
  // type parameters.
  object introWithoutImplicit {
    def sort[T](xs: List[T])(ordT: Ord[T]): List[T] = xs match {
      case Nil => Nil
      case x :: xs => {
        val lesser = xs.filter(ordT.compare(_,x))
        val greater = xs.filter(!ordT.compare(_,x))

        sort(lesser)(ordT) ++ List(x) ++ sort(greater)(ordT)
      }
    }

    trait Ord[T] {
      def compare(a: T, b: T): Boolean
    }

    object intOrd extends Ord[Int] {
      override def compare(a: Int, b: Int) = a <= b
    }

    // scala> import OMO10a.introWithoutImplicit._
    // scala> sort(List(2,3,1))(intOrd)
    // res0: List[Int] = List(1, 2, 3)
  }

  object introWithImplicit {
    // Notice implicit in front of ordT
    def sort[T](xs: List[T])(implicit ordT: Ord[T]): List[T] = xs match {
      case Nil => Nil
      case x :: xs => {
        val lesser = xs.filter(ordT.compare(_,x))
        val greater = xs.filter(!ordT.compare(_,x))

        // ordT is no more required there
        sort(lesser) ++ List(x) ++ sort(greater)
      }
    }

    trait Ord[T] {
      def compare(a: T, b: T): Boolean
    }

    // Notice implicit in front of intOrd object
    implicit object intOrd extends Ord[Int] {
      override def compare(a: Int, b: Int) = a <= b
    }

    // scala> // ordT is no more required in sort:
    // scala> import OMO10a.introWithImplicit._
    // scala> sort(List(2,3,1))
    // res0: List[Int] = List(1, 2, 3)
  }

  // Second, the programmer is free to provide an explicit argument.
  // This argument becomes part of the implicit scope so that implicit
  // argument is propagated naturally.
  object implicitsInScala {
    import java.io.PrintStream

    def logTm(msg: String)(implicit o: PrintStream): Unit =
      log("[" + new java.util.Date() + "] " + msg)

    def log(msg: String)(implicit o: PrintStream): Unit =
      o.println(msg)

    // scala> import OMO10a.implicitsInScala._
    // scala> implicit val out = System.out
    // scala> logTm("Does not compute!")
    // [Thu Jan 29 16:57:09 CET 2015] Does not compute!
    // scala> logTm("Does not compute!")(System.err)
    // Err: [Thu Jan 29 16:57:15 CET 2015] Does not compute!
   }

  // Problem is, when you want to pass mutliple implicit arguments and
  // fix one of them, because implicit argument list may either be
  // omitted or supplied in its entierly. However there is a simple
  // idiom to encode a wildcard for an implicit argument.
  object implicitsWildcard {
    import java.io.PrintStream

    // LogTm generalization so that we can specify implicitly an
    // arbitrary output and an arbitrary prefix.
    def logPrefix(msg: String)(implicit
                               o: PrintStream,
                               prefix: String): Unit =
      log("[" + prefix + "] " + msg)

    def log(msg: String)(implicit o: PrintStream): Unit =
      o.println(msg)

    // Polymorphic method which look up an implicit value for type T
    // in the implicit scope.
    def ?[T](implicit w: T): T = w

    // scala> import OMO10a.implicitsWildcard._
    // scala> implicit val out = System.out
    // scala> // When we want to fix a implicit paramter, the entire implicit
    // scala> // argument list must be supplied. Thus, without wildcard, if we
    // scala> // want to fix `prefix`, we also have to fix `output`.
    // scala> logPrefix("Does not compute!")(out, (new java.util.Date()).toString())
    // [Thu Jan 29 17:54:53 CET 2015] Does not compute!
    // scala> // With wildcard, we can omitting the value for the `output`,
    // scala> // while providing an explicit value for the `prefix`. Type
    // scala> // inference and implicit search will turn the `?` into
    // scala> // `?[PrintStream](out)`.
    // scala> logPrefix("Does not compute!")(?, (new java.util.Date()).toString())
    // [Thu Jan 29 17:54:53 CET 2015] Does not compute!
    // scala> // In scala, the `?[T]` methods is available as
    // scala> // scala.Predef.implicilty
    // scala> logPrefix("Does not compute!")(implicitly,
    // scala>                                (new java.util.Date()).toString())
    // [Thu Jan 29 17:54:53 CET 2015] Does not compute!
  }

}


/** Second example, Nat
  *
  * [[https://markehammons.wordpress.com/2013/07/02/dependent-types-in-scala/]]
  * [[https://github.com/milessabin/shapeless/blob/shapeless-2.0.0/core/src/main/scala/shapeless/nat.scala]]
  * [[https://github.com/milessabin/shapeless/blob/shapeless-2.0.0/core/src/main/scala/shapeless/ops/nat.scala]]
  */

// Nat is a trait indicating a naturel number, and Succ is a subtype
// of it that takes a Nat as a type parameter.
trait Nat

trait Succ[P <: Nat] extends Nat

object Nat {
  trait _0 extends Nat
  implicit object _0 extends _0

  type _1 = Succ[_0]
  implicit val _1 = new _1 {}

  type _2 = Succ[_1]
  implicit val _2 = new _2 {}
}

// We don't define operation for our Nats as methods on our types, but
// rather as types themselves. Below is the definition of the
// dependent type `Pred`, which takes a Num and returns it's
// predecessor as that number decremented by one.

// Pred is a Type class that witnesses that `Out` is the predecessor
// of `N`. Pred depends on another type class called PredAux that
// defines it's behavior. It's used with the style
// `implicitly[Pred[_2]]` which returns a Pred trait with an `Out`
// type of `_1`. But how does it go from an implicit definition that
// takes two generic parameters to determine the predecessor of our
// number? Look at `PredAux` to find the answer.


trait Pred[N <: Nat] {
  type Out <: Nat
}

// PredAux is a type class that takes two parameters N and P, with P
// the predecessor of N. When you pass a typeclass (e.g.:
// `PredAux[N,P]`) with type arguments (e.g.: in `Pred.pred`), scala
// compiler searches for an implicit in the companion object (e.g.:
// `PredAux`) that has the same type as the type passed into implicit
// (e.g.: `PredAux.pred`). In this case, if I do
// `implicitly[PredAux[_1, _0]]`, the compiler will use the implicit
// definition `PredAux.pred` to instantiate our type class. And, if I
// try `implicitly[PredAux[_2, _0]]`, the compiler will complain about
// not being able to find an implicit value for the parameter, because
// there is no implicit definition in `PredAux` with a signature
// `PredAux[Succ[Succ[N]], N]`.
trait PredAux[N <: Nat, P <: Nat]

// Implements the behavior of Pred. The implicit method instantiate a
// `PredAux` for a given Nat.
object PredAux {
  implicit def pred[N <: Nat]: PredAux[Succ[N], N] = new PredAux[Succ[N], N] {}
}

// When we use Pred, we want to pass it one type parameter (e.g.:
// `implicitly[Pred[_1]]`), and have it to work on what the type of
// `Out` is. The trick here is when we call `implicitly[Pred[_1]]`,
// the compiler try to match on `Pred.pred` because of it's type
// `Pred[N]`, and the parameter `P` is calculated by the compiler
// thanks to the implicit parameter `PredAux`.
object Pred {
  // For `implicitly[Pred[_2]]` The compiler knows type `N` is
  // `Succ[Succ[_0]]`, it can then infer that `P` is `Succ[_0]`.
  implicit def pred[N <: Nat, P <: Nat](implicit p: PredAux[N,P]): Pred[N] = new Pred[N] {
    type Out = P
  }
}

// Thus, Pred is a Π-type it takes a type `N =:= Succ[_ <: Nat]` and
// return a type `P` that is type `N` predecessor.
