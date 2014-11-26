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

object LCH11a extends App {
  /** Custom implementation of Hugues Arrows. */
  object alaronan {
    // `F' stands for Functor
    trait Arrow[F[_,_],B,C] {
      /** Current Arrow */
      def self: F[B,C]

      def run(x: B): C

      /** Lifts an ordinary function.
        *
        * {{{
        *    b +--arr--+ c
        *   ---|-> f --|--->
        *      +-------+
        * }}}
        *
        */
      def arr[D,E](f: (D) => E): Arrow[F,D,E]

      /** Composes two arrow computations by connecting the output of the
        * first to the input of the second.
        *
        * {{{
        *      +--------------compose-------------+
        *    b |  b +----arr---+  c  +--arr--+ d  | d
        *   ---|----|-> self --|-----|-> k --|----|--->
        *      |    +----------+     +-------+    |
        *      +----------------------------------+
        * }}}
        *
        */
      def compose[D](k: Arrow[F,C,D]): Arrow[F,B,D]

      /** Alias for compose. */
      def >>>[D](k: Arrow[F,C,D]): Arrow[F,B,D] = compose(k)

      /** Converts an arrow computation taking one input and one result,
        * into an arrow computation taking two inputs and two results. The
        * original arrow is applied to the first part of the input, and
        * the result becomes the first part of the output. The second part
        * of the input is fed directly to the second part of the output.
        *
        * {{{
        *      +--------first-------+
        *    b |  b +----arr---+ c  | c
        *   ---|----|-> self --|----|--->
        *    d |    +----------+    | d
        *   ---|--------------------|--->
        *      +--------------------+
        * }}}
        *
        */
      def first[D]: Arrow[F,(B, D),(C, D)]

      /** Dual of first.
        *
        * {{{
        *    d +-------second-------+ d
        *   ---|--------------------|--->
        *    b |  b +---arr----+ c  | c
        *   ---|----|-> self --|----|--->
        *      |    +----------+    |
        *      +--------------------+
        * equiv:
        *      +------------------------second------------------------------+
        *      |                   +---------first------+                   |
        *    d | d +---arr--+ b  b |  b +----arr---+ c  | c  c +---arr--+ d | d
        * -----|---|->    --|------|----|-> self --|----|------|->    --|---|--->
        *    b | b |  swap  | d  d |  d +----------+ d  | d  d |  swap  | c | c
        * -----|---|->    --|------|--------------------|------|->    --|---|--->
        *      |   +--------+      +--------------------+      +--------+   |
        *      +------------------------------------------------------------+
        * }}}
        *
        */
      def second[D]: Arrow[F,(D, B),(D, C)] = {
        // swap in a polymorphic manner
        def swap[X,Y](t: (X, Y)): (Y, X) = t.swap
        def swapArrow[X,Y]: Arrow[F,(X, Y),(Y, X)] = arr(swap _)

        swapArrow[D,B] >>> first >>> swapArrow[C,D]
      }

      /** Parrallel composition is the sequence of first and second.
        *
        * {{{
        *     +---------------split-----------+
        *   b | b +----arr---+ c              | c
        *  ---|---|-> self --|----------------|---->
        *   d |   +----------+  d +--arr--+ e | e
        *  ---|-------------------|-> k --|---|---->
        *     |                   +-------+   |
        *     +-------------------------------+
        * }}}
        *
        */
      def split[D,E](g: Arrow[F,D,E]): Arrow[F,(B, D),(C, E)] =
        first >>> g.second

      /** Alias for split */
      def ***[D,E](g: Arrow[F,D,E]): Arrow[F,(B, D),(C, E)] =
        split(g)
    }

    object Arrow {
      def apply[B,C](v: Function1[B,C]): Arrow[Function1,B,C] = new Arrow[Function1,B,C] {
        def self: B => C = v
        def run(x: B): C = self(x)
        def arr[D, E](f: D => E): Arrow[Function1,D,E] = Arrow(f)
        def compose[D](k: Arrow[Function1,C,D]): Arrow[Function1,B,D] =
          Arrow(self andThen k.self)
        def first[D]: Arrow[Function1,(B, D),(C, D)] =
          Arrow({ case (b,d) => (self(b),d) })
      }
    }
  }

  /** Scalaz implementation of Hugues' Arrows */
  object alascalaz {
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

    class ArrowOps[=>:[_, _], B, C](val self: B =>: C)(implicit val F: Arrow[=>:]) {
      final def >>>[D](k: C =>: D): B =>: D =
        F.compose(self, k)

      final def first[D]: (B,D) =>: (C,D) =
        F.first(self)

      final def second[D]: (D,B) =>: (D,C) =
        F.second(self)

      final def ***[D,E](k: D =>: E): (B,D) =>: (C,E) =
        F.split(self, k)
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

      implicit def tof1arrowop[=>:[_, _]: Arrow, B, C](v: B =>: C): ArrowOps[=>:,B,C] =
        new ArrowOps[=>:,B,C](v)
    }
  }

  object ArrowTests {
    val plus1 = (_: Int) + 1
    val times2 = (_: Int) * 2
    val rev = (_: String) reverse

    def alascalaztests = {
      import alascalaz._
      import alascalaz.Arrow._
      val Arrow = implicitly[Arrow[Function1]]

      // With arrow container
      Arrow.compose(
        Arrow.split(
          Arrow.compose(Arrow.arr(plus1), Arrow.arr(times2)),
          Arrow.arr(rev)),
        Arrow.arr(println)) apply (7, "abc")

      // With arrow operations
      ((plus1 >>> times2) *** rev) >>> println apply (7, "abc")
    }

    def alaronantests = {
      import alaronan._
      import alaronan.Arrow._

      ((Arrow(plus1) >>> Arrow(times2)) *** Arrow(rev)) >>> Arrow(println) run (7, "abc")
    }
  }

  ArrowTests.alascalaztests
  ArrowTests.alaronantests
}
