/** Functional Programming in Scala, Chapter 6 */
package fpinscala

trait RNG { def nextInt: (Int, RNG) }
case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = Simple(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt

    ((i1, i2) -> rng3)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt

    (if (i == Int.MinValue) Int.MaxValue
     else Math.abs(i),
     rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)

    (i / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng2)

    ((i, d) -> rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = nonNegativeInt(rng2)

    ((d, i) -> rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)

    ((d1, d2, d3) -> rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (i, rng2) = rng.nextInt
      val (l, rng3) = ints(count - 1)(rng2)
      (i :: l, rng3)
    } else (Nil, rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  // Passes the RNG state through without using it.
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // Transforms the *output* of a state action without modifying the
  // state itself.
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    (rng: RNG) => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // Using map
  def double_2: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A])(rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  // Using map2
  def both[A,B](ra: Rand[A])(rb: Rand[B]): Rand[(A, B)] =
    map2(ra)(rb)((_, _))

  def intDouble_2: Rand[(Int, Double)] =
    both(nonNegativeInt)(double)

  def doubleInt_2: Rand[(Double, Int)] =
    both(double)(nonNegativeInt)

  def randIntDouble: Rand[(Int, Double)] =
    both(int)(double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double)(int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil:List[A]))(
      (h, rest) => map2(h)(rest)(_ :: _))

  def ints_2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))
}



object FPInScalaStateTest extends App {
  val rng = Simple(42)
  val (i, rng2) = RNG.randomPair(rng); println(i)
  val (i2, rng3) = RNG.nonNegativeInt(rng2); println(i2)
  val (d, rng4) = RNG.double(rng3); println(d)
  val (p1, rng5) = RNG.intDouble(rng4); println(p1)
  val (p2, rng6) = RNG.doubleInt(rng5); println(p2)
  val (t3, rng7) = RNG.double3(rng6); println(t3)
  val (l, rng8) = RNG.ints(5)(rng7); println(l)
  val (i3, rng9) = RNG.nonNegativeEven(rng8); println(i3)
  val (d2, _) = RNG.double(rng3); println(d2)
  val (p3, _) = RNG.intDouble_2(rng4); println(p3)
  val (p4, _) = RNG.doubleInt_2(rng5); println(p2)
  val (p5, _) = RNG.randIntDouble(rng4); println(p5)
  val (p6, _) = RNG.randDoubleInt(rng5); println(p6)
  val (l2, _) = RNG.ints(5)(rng7); println(l2)
}
