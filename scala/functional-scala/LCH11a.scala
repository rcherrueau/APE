// @Article{LCH11a,
//   author =       {Hai Liu and Eric Cheng and Paul Hudak},
//   title =        {Causal commutative arrows},
//   journal =      {J. Funct. Program.},
//   volume =       21,
//   number =       {4-5},
//   pages =        {467--496},
//   year =         2011,
//   url =          {http://dx.doi.org/10.1017/S0956796811000153},
//   doi =          {10.1017/S0956796811000153},
//   timestamp =    {Mon, 12 Sep 2011 17:55:56 +0200}
// }


// `=>:' stands for Function1
trait Arrow[=>:[_,_]] {
  /** Lifts an ordinary function.
    *
    * {{{
    *    b +--arr--+ c
    *   ---|-> f --|--->
    *      +-------+
    * }}}
    *
    */
  def arr[B,C](f: (B) => C): B =>: C

  /** Composes two arrow computations by connecting the output of the
    * first to the input of the second.
    *
    * {{{
    *      +-----------compose-------------+
    *    b |  b +--arr--+  c  +--arr--+ d  | d
    *   ---|----|-> f --|-----|-> g --|----|--->
    *      |    +-------+     +-------+    |
    *      +-------------------------------+
    * }}}
    *
    */
  def compose[B,C,D](f: B =>: C, g: C =>: D): B =>: D

  /** Converts an arrow computation taking one input and one result,
    * into an arrow computation taking two inputs and two results. The
    * original arrow is applied to the first part of the input, and
    * the result becomes the first part of the output. The second part
    * of the input is fed directly to the second part of the output.
    *
    * {{{
    *      +------first------+
    *    b |  b +--arr--+ c  | c
    *   ---|----|-> f --|----|--->
    *    d |    +-------+    | d
    *   ---|-----------------|--->
    *      +-----------------+
    * }}}
    *
    */
  def first[B,C,D](f: B =>: C): (B,D) =>: (C,D)

  /** Dual of first.
    *
    * {{{
    *    c +-----second------+ c
    *   ---|-----------------|--->
    *    d |  d +--arr--+ e  | e
    *   ---|----|-> f --|----|--->
    *      |    +-------+    |
    *      +-----------------+
    * equiv:
    *      +---------------------second------------------------------+
    *      |                   +------first------+                   |
    *    c | c +---arr--+ d  d |  d +--arr--+ e  | e  e +---arr--+ c | c
    * -----|---|->    --|------|----|-> f --|----|------|->    --|---|--->
    *    d | d |  swap  | c  c |  c +-------+ c  | c  c |  swap  | e | e
    * -----|---|->    --|------|-----------------|------|->    --|---|--->
    *      |   +--------+      +-----------------+      +--------+   |
    *      +---------------------------------------------------------+
    * }}}
    *
    */
  def second[C,D,E](f: D =>: E): (C,D) =>: (C,E) = {
    // swap in a polymorphic manner
    def swap[X,Y](t: (X,Y)): (Y,X) = t.swap

    compose(arr(swap[C,D] _), compose(first[D,E,C](f), arr(swap[E,C] _)))
  }

  /** Parrallel composition is the sequence of first and second.
    *
    * {{{
    *      +------------split-------------+
    *   b1 | b1 +--arr--+ b2              | b2
    *  ----|----|-> f --|-----------------|---->
    *   c1 |    +-------+ c1 +--arr--+ c2 | c2
    *  ----|-----------------|-> g --|----|---->
    *      |                 +-------+    |
    *      +------------------------------+
    * }}}
    *
    */
  def split[B1,B2,C1,C2](f: B1 =>: B2, g: C1 =>: C2): (B1,C1) =>: (B2,C2) =
    compose(first[B1,B2,C1](f), second[B2,C1,C2](g))
}

object Arrow {
  def apply[F[_,_]: Arrow]: Arrow[F] = implicitly[Arrow[F]]

  implicit val f1arrow = new Arrow[Function1] {
    def arr[B, C](f: B => C): B => C = f
    def compose[B, C, D](f: B => C,g: C => D): B => D =
      f andThen g
    def first[B, C, D](f: B => C): ((B, D)) => (C, D) = {
      case (b,d) => (f(b),d)
    }
  }

  object TestArrow extends App {
    // implicit def f1ToArrow(f: Function1[_, _]): Arrow[Function1] =
    //   implicitly[Arrow[Function1]].arr(f)

    val plus1 = (_: Int) + 1
    val times2 = (_: Int) * 2
    val rev = (_: String) reverse

    implicitly[Arrow[Function1]].first(implicitly[Arrow[Function1]].arr(plus1)) apply (7, "abc")

    // plus1.first apply (7, "abc")
  }

}
