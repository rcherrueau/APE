//
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
  /** Lifts an ordinary function */
  def arr[A,B](f: (A) => B): A =>: B

  /** Composes two arrow computations by connecting the output of the
    * first to the input of the second. f(g())
    */
  def compose[A,B,C](f: B =>: C, g: A =>: B): A =>: C

  /** Converts an arrow computation taking one input and one result,
    * into an arrow computation taking two inputs and two results. The
    * original arrow is applied to the first part of the input, and
    * the result becomes the first part of the output. The second part
    * of the input is fed directly to the second part of the output.
    */
  def first[A,B,C](f: A =>: B): (A,C) =>: (B,C)

  /** Dual of first */
  def second[A,B,C](f: A =>: B): (C,A) =>: (C,B) = {
    // swap in a polymorphic manner
    def swap[X,Y](t: (X,Y)): (Y,X) = t.swap

    compose(arr(swap[B,C] _), compose(first[A,B,C](f), arr(swap[C,A] _)))
  }

  /** Parrallel composition is the sequence of first and second */
  def seq[A,B,C,D](f: A =>: B, g: C =>: D): (A,C) =>: (B,D) =
    compose(first[A,B,C](f), second[C,D,B](g))
}
