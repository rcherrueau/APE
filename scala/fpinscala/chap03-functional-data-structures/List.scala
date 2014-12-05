/** Functional Programming in Scala, Chapter 3 */
package fpinscala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], newH: A): List[A] = l match {
    case Nil => sys.error("setHead on empty List")
    case Cons(h, t) => Cons(newH, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if (n > 0) => drop(t, n - 1)
    case _ => l
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  /** All but the last element */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  // Use foldRight in there implementations:
  def sum_2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def product_2(ns: List[Double]): Double = foldRight(ns, 1.)(_ * _)

  def length[A](l: List[A]): Int = foldRight(l, 0)((x,y) => (1 + y))

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z,h))(f)
  }

  // Use foldLeft in there implementations:
  def sum_3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product_3(ns: List[Double]): Double = foldLeft(ns, 1.)(_ * _)

  def length_2[A](l: List[A]): Int = foldLeft(l, 0)((acc,h) => 1 + acc)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((acc, h) => Cons(h, acc))

  // foldRight via foldLeft => foldRight_2 is tailrec but go trhough
  // the list twice.
  def foldRight_2[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((acc,h) => f(h,acc))


  // foldLeft via foldRight => foldLeft_2 is no more tailrec but keeps
  // elements of the list in the right order.
  def foldLeft_2[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((h,g) => b => g(f(b,h)))(z)

  def append[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_,_))

  def flat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def add1(ints: List[Int]): List[Int] =
    foldRight(ints, Nil:List[Int])((h,rest) => Cons(h+1, rest))

  def double2String(ds: List[Double]): List[String] =
    foldRight(ds, Nil:List[String])((h, rest) => Cons(h.toString, rest))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((h, rest) => Cons(f(h), rest))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((h, rest) => if (f(h)) Cons(h, rest)
                                            else rest)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    flat(map(as)(f))

  def filter_2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] =
    // Make a tuple2 to pattern match over the two:
    (as, bs) match {
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a,b), zipWith(as,bs)(f))
      case _ => Nil
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sub match {
      case Cons(h, t) =>
        def allOccures(l: List[A], e: A): List[List[A]] = l match {
          case Cons(h, t) if (h == e) => Cons(l, allOccures(t, e))
          case Cons(h, t) => allOccures(t, e)
          case Nil => Nil
        }

        def same(l: List[A], ll: List[A]): Boolean = (l,ll) match {
          case (Cons(h, t), Cons(hh, tt)) => (h == hh) && same(t, tt)
          case (_, Nil) => true
          case (Nil, _) => false
        }

        def step(l: List[List[A]]): Boolean = l match {
          case Cons(l, ll) => same(l, sub) || step(ll)
          case Nil => false
        }

        step(allOccures(sup, h))
      case Nil => false
    }
}

object FPInScalaListTest extends App {
  println(List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, (Cons(4, _))))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  })

  println(List.drop(List(1,2,3,4,5), 3))
  println(List.dropWhile(List(1,2,3,4,5))(_ <= 3))
  println(List.init(List(1,2,3,4,5)))
  println(List.foldRight(List(1,2,3,4,5), Nil:List[Int])(Cons(_,_)))
  println(List.length(List(1,2,3,4,5)))
  println(List.foldLeft(List(1,2,3,4,5), 0)(_ + _))
  println(List.foldLeft(List(1,2,3,4,5), Nil:List[Int])(
            (ll,h) => Cons(h,ll)))
  println(List.reverse(List(1,2,3,4,5)))
  println(List.foldLeft_2(List(1,2,3,4,5), 0)(_ + _))
  println(List.foldLeft_2(List(1,2,3,4,5), Nil:List[Int])(
            (ll,h) => Cons(h,ll)))
  println(List.foldRight_2(List(1,2,3,4,5), Nil:List[Int])(Cons(_,_)))
  println(List.append(List(1,2,3,4,5), List(1,2,3,4,5)))
  println(List.flat(List(List(1,2,3,4,5), List(1,2,3,4,5))))
  println(List.add1(List(1,2,3,4,5)))
  println(List.double2String(List(1.,2.,3.,4.,5.)))
  println(List.map(List(1.,2.,3.,4.,5.))(_.toString))
  println(List.filter(List(1,2,3,4,5))(_ % 2 == 0))
  println(List.flatMap(List(1,2,3,4,5))(i => List(i,i)))
  println(List.filter_2(List(1,2,3,4,5))(_ % 2 == 0))
  println(List.zipWith(List(1,2,3,4,5),
                       List("a","b","c","d"))(
            (i,s) => i.toString + s))
  println(List.hasSubsequence(List(1,2,3,4), List(1,2)))
  println(List.hasSubsequence(List(1,2,3,4), List(2,3)))
  println(List.hasSubsequence(List(1,2,3,4), List(1)))
  println(List.hasSubsequence(List(1,2,3,4), List(4)))
  println(List.hasSubsequence(List(1,2,3,4), List(1,3)))
  println(List.hasSubsequence(List(1,2,3,4), List(2,4)))
  println(List.hasSubsequence(List(1,2,3,4), List(5,6,7)))
  println(List.hasSubsequence(List(1,2,3,4), List()))
}
