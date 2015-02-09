/** Functional Programming in Scala, Chapter 5 */
package fpinscala

// Reminder:
// - Scala on function is call-by-value.
// - Thunk `a: => A' for non-strict function is call-by-name.
// - Lazy value `lazy val a' is memoized, thus lazy val on
//   call-by-name implements a sort of call-by-need.

/** Lazy List or Stream. */
sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h()) // We don't evaluate the tail of the
                                 // Stream.
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] =
    if (n > 0) this match {
      case Cons(h, t) if (n  > 1) => cons(h(), t().take(n - 1))
      case Cons(h, t) if (n == 1) => cons(h(), empty)
      case _ => empty
    }
    else Stream()

  def drop(n: Int): Stream[A] =
    if (n > 0) this match {
      case Cons(h, t) => t().drop(n - 1)
      case Empty => empty
    }
    else this

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  // The arrow => in front of the argument of type B in f means that
  // the function f takes its second argument (the rest of the
  // computation) by-name.
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    // if `f' chooses not to evaluate the second argument, this
    // terminate the traversal early.
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // Using foldRight

  // The early termination of foldRight is visible in the following
  // implementation of exists. If `p' returns true, the `rest' is not
  // evaluated and the computation is aborted.
  def exists_2(p: A => Boolean): Boolean =
    foldRight(false)((h, rest) => p(h) || rest)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, rest) => p(h) && rest)

  // Stack safe forAll
  def forAll_2(p: A => Boolean): Boolean = {
    @annotation.tailrec
    def step(s: Stream[A], res: Boolean): Boolean = s match {
      case Cons(h, t) => step(t(), p(h()))
      case _ => false
    }

    step(this, true)
  }

  def takeWhile_2(p: A => Boolean): Stream[A] =
    foldRight(empty:Stream[A])(
      (h, rest) => if (p(h)) cons(h, rest) else empty)

  def headOption_2: Option[A] =
    // This one is OK because `rest' is by-name and the recursion
    // never happens:
    foldRight(None:Option[A])((h, rest) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, rest) => cons(f(h), rest))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty:Stream[A])(
      (h, rest) => if (p(h)) cons(h, rest) else rest)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, rest) => cons(h, rest))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, rest) => f(h) append rest)

  // Because we are lazy, we can reuse `filter' to define `find' that
  // returns just the first element that matches if it exists. Even
  // though `filter' transforms the whole stream, that transformation
  // is done lazily, so `find' terminates as soon as a match is found.
  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  // Using unfold

  def map_2[B](f: A => B): Stream[B] =
    unfold(this)(_ match {
                   case Cons(h, t) => Some(f(h()), t())
                   case _ => None
                 })

  def take_2(n: Int): Stream[A] =
    unfold((this, n))(_ match {
                        case (Cons(h, t), 1) =>
                          Some(h(), (empty, n-1))
                        case (Cons(h, t), n) if (n > 1) =>
                          Some(h(), (t(), n-1))
                        case _ => None
                      })

  def takeWhile_3(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if (p(h())) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()) -> (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      // Use arrow `->' to construct tuple with a more clearer syntax
      case (Empty, Cons(h, t)) => Some(((None, Some(h())) ->  (empty, t())))
      case (Cons(h, t), Empty) => Some(((Some(h()), None) -> (t(), empty)))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(((Some(h1()), Some(h2())) -> (t1(), t2())))
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (a, b) => a == b
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case s@Cons(h, t) => Some(s, t())
      case Empty => None
    } append Stream.empty

  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)
}

/** Empty Stream. */
case object Empty extends Stream[Nothing]

/** Nonempty Stream.
  *
  * A nonempty Stream consists of a head `h' and a tail `t', which are
  * both non-strict.
  *
  * Due to technical limitations, these are thunks that must be
  * explicitly forced, rather than by-name parameters.
  */
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  // Smart constructor by convention has lowercases the first latter
  // of the corresponding data constructor. Here, `cons' takes care of
  // memoizing call-by-name arguments.
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // memoization:
    lazy val head = hd
    lazy val tail = tl
    // thunk will only do its work once, when forced for the first
    // time. Subsequent forces will return the cached lazy val:
    Cons(() => head, () => tail)
  }

  // The empty smart constructor returns `Empty' annotated as
  // `Stream[A]', which is better for type inference.
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    // Argument of cons are in thunks, so the `as.head' and
    // `apply(as.tail: _*)' expressons won't be evaluated until we
    // force the Stream. /!\ False see,
    // https://github.com/fpinscala/fpinscala/issues/321 try
    // `Stream({println("One"); 1}, {println("Two"); 2},
    // {println("Three"); 3})' scala Stream manages this using the
    // `#::' operator/constructor.
    else cons(as.head, apply(as.tail: _*))

  // scala solution:
  // https://github.com/scala/scala/blob/v2.11.4/src/library/scala/collection/immutable/Stream.scala#L1177
  // But this evaluate the hd
  implicit class ConsWrapper[A](tl: => Stream[A]) {
    def #::(hd: => A): Stream[A] = cons(hd, tl)
  }

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def step(curr: Int = 0, next: Int = 1): Stream[Int] =
      cons(curr, step(next, curr + next))

    step()
  }

  // Takes an initial state `z' and a function for produciong both the
  // next state and the next value in the generated stream.
  def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) map { case(a, s) => cons(a, unfold(s)(f)) } getOrElse empty

  // Using unfold corecursive function
  def constant_2[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a,a))

  def from_2(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n+1))

  def fibs_2: Stream[Int] =
    unfold((0,1)) { case (curr, next) => Some(curr, (next, curr+next)) }
}

object FPInScalaStreamTest extends App {

  def test(s: => Stream[Int]): Unit = {
    println(s.toList)
    println(s.toList)
    println(s.take(2).toList)
    println(s.take(4).toList)
    println(s.drop(2).toList)
    println(s.drop(4).toList)
    println(s.takeWhile(_ <= 2).toList)
    println(s.takeWhile(_ <= 4).toList)
    println(s.exists(_ < 3))
    println(s.exists(_ < 0))
    println(s.exists_2(_ < 3))
    println(s.exists_2(_ < 0))
    println(s.forAll(_ < 4))
    println(s.forAll(_ < 2))
    println(s.takeWhile_2(_ <= 2).toList)
    println(s.takeWhile_2(_ <= 4).toList)
    // With side-effect stream, look how it doesn't consume the full
    // list but still apply map on taken elements:
    println(s.map(_ + 10).take(2).toList)
    println(s.map_2(_ + 10).take(2).toList)
    // Here, because we don't do a toList, this apply filter on head
    // of the steam and maybe apply filter on the rest if we ask
    // latter.
    val sf = s.filter(_ % 2 == 0)
    println(sf)
    println(sf.toList)
    println(s append "a" #:: "b" #:: "c" #:: Stream.empty)
    println((s append "a" #:: "b" #:: "c" #:: Stream.empty).toList)
    println((s flatMap { x => x #:: Stream.empty }).toList)
    println(s.zipWith("a" #:: "b" #:: "c" #:: "d" #:: Stream.empty)(
              (i, s) => i.toString + s).toList)
    println((s zipAll "a" #:: "b" #:: "c" #:: "d" #:: Stream.empty).toList)
    println((s zipAll "a" #:: "b" #:: "c" #:: Stream.empty).toList)
    println(("a" #:: "b" #:: "c" #:: Stream.empty zipAll(s)).toList)
    println(s startsWith s)
    println(s startsWith s.take(2))
    println(s startsWith Stream.empty)
    println(s startsWith "a" #:: "b" #:: "c" #:: Stream.empty)
    println(s.tails.map { _.toList }.toList)
    println(s hasSubsequence s)
    println(s hasSubsequence 2 #:: 3 #:: Stream.empty)
    println(s hasSubsequence 4 #:: Stream.empty)
  }

  def p[A](x: A) = print(x)

  // `Stream.apply' isn't call by name, use `#::' instead:
  Stream({p("One -> "); 1},
         {p("Two -> "); 2},
         {p("Three -> "); 3},
         {p("Four -> "); 4})
  println()

  Stream.cons({p("One -> "); 1},
              Stream.cons({p("Two -> "); 2},
                          Stream.empty))
  println()

  {p("One -> "); 1} #:: {p("Two -> "); 2} #::
                        {p("Three -> "); 3} #::
                        {p("Four -> "); 4} #::
                        Stream.empty
  println()


  // No side-effect stream tests:
  println("\nSome tests with no side-effect stream: ")
  test(1 #:: 2 #:: 3 #:: 4 #:: Stream.empty)

  // Side-effect stream tests:
  println("\nSome tests with side-effect stream: ")
  test(Stream.cons({p("One -> "); 1},
         Stream.cons({p("Two -> "); 2},
           Stream.cons({p("Three -> "); 3},
             Stream.cons({p("Four -> "); 4},
                         Stream.empty)))))

  /** Trace of that the lazy execution.
    *
    * Expression surrounded by `*' is not evaluated
    *
    * {{{
    * Stream(1, 2, 3, 4).map(_ + 10).take(2).toList
    * # apply map(_ + 10)
    * cons(1 + 10, *Stream(2, 3, 4).map(_ + 10)*).take(2).toList
    * cons(11, *Stream(2, 3, 4).map(_ + 10)*).take(2).toList
    * # apply taken(2)
    * cons(11, *Stream(2, 3, 4).map(_ + 10).take(1)*).toList
    * # apply toList
    * 11 :: Stream(2, 3, 4).map(_ + 10).take(1).toList
    * # apply map(_ + 10)
    * 11 :: cons(2 + 10, *Stream(3, 4).map(_ + 10)*).take(1).toList
    * 11 :: cons(12, *Stream(3, 4).map(_ + 10)*).take(1).toList
    * # apply take(1)
    * 11 :: cons(12, *empty*).toList
    * # apply toList
    * 11 :: 12 :: empty.toList
    * 11 :: 12 :: Nil
    * }}}
    */
  Stream(1,2,3,4).map(_ + 10).take(2).toList == List(11,12)

  println("\nInfinite streams:")
  val ones: Stream[Int] = Stream.cons(1, ones)
  println(ones.take(5).toList)
  println(ones.exists(_ % 2 != 0))
  println(ones.map(_ + 1).exists(_ % 2 == 0))
  println(ones.takeWhile(_ == 1)) // OK
  // println(ones.takeWhile(_ == 1).toList) // Stack Overflow!
  println(ones.forAll(_ != 1))
  // println(ones.forAll(_ == 1)) // Stack Overflow!
  // println(ones.forAll_2(_ == 1)) // Infiniite loop with stack safe
  //                                // `forAll'
  println(Stream.constant("42").take(42).toList)
  println(Stream.constant_2("42").take(42).toList)
  println(Stream.from(1).take(42).toList)
  println(Stream.from_2(1).take(42).toList)
  println(Stream.fibs.take(42).toList)
  println(Stream.fibs_2.take(42).toList)
  println(Stream.fibs.take_2(42).toList)
  println(Stream.fibs_2.take_2(42).toList)
  println(ones.takeWhile_3(_ == 1)) // OK
}
