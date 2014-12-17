/** Functional Programming in Scala, Chapter 7 */
package fpinscala

object FunctionalAPIMethodology {
  // Functional API Methodology:

  // First, right a program in which you want bring parallelism.
  // => Sum with divide and conquer algorithm.
  def sum_1(ints: Seq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      sum_1(l) + sum_1(r)
    }

  // Then, look at the program and try to make parallelism explicit.
  // In `sum_1' we can see that any data type we might choose to
  // represent our parallel computations needs to be able to contain a
  // *result*.
  def sum_2(ints: Seq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      // Compute the left half in parallel
      val sumL: Par_2[Int]  = Par_2.unit(sum_2(l))
      // Compute the right half in parallel
      val sumR: Par_2[Int] = Par_2.unit(sum_2(r))
      // Extract both results and sums them.
      Par_2.get(sumL) + Par_2.get(sumR)
    }
  // For the version 2, we choose that `unit` evaluates arguments
  // immediatly in a separate thread and `get` returns result.
  trait Par_2[A]
  object Par_2 {
    /** Takes an unevaluated `A' and returns a computation that might
      * evaluate it in a separate thread */
    // `f' have to be lazy. `Par' begins immediate execution in a
    // parallel thread.
    def unit[A](f: => A): Par_2[A] = ???
    /** Extracts the resulting value from a parallel computation */
    def get[A](par: Par_2[A]): A = ???
  }

  // Let's see if we can avoit the aforementioned pitfall of combining
  // `unit' and `get'.
  // If we don't call `get', that implies that our `sum' function must
  // return a `Par[Int]'.
  def sum_3(ints: Seq[Int]): Par_3[Int] =
    if (ints.size <= 1)
      Par_3.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par_3.map2(sum_3(l), sum_3(r)) { _ + _ }
    }
  trait Par_3[A]
  object Par_3 {
    def unit[A](f: => A): Par_3[A] = ???
    /** Combine the result of two parallel computations */
    // `p1' and `p2' have to be lazy. `Par' begins immediate executions
    // of both side in parallel threads.
    def map2[A,B,C](p1: => Par_3[A], p2: => Par_3[B])(f: (A,B) => C): Par_3[C] = ???
  }

  // But, sometimes we don't want to evaluate the two arguments to
  // `map2' in parallel. Consider this simple hypotetical example:
  // `Par.map2(Par.unit(1), Par.unit(1)) { _ + _ }' In this case, the
  // two computations we're combining will execute so quickly that
  // there isn't much point in spawning off a thread to evaluate them.
  // For this reason, we want to make the forking more explicit. We
  // can do that by inventing another function which can take to mean
  // that the Par should be run in a separate thread.
  def sum_4(ints: Seq[Int]): Par_4[Int] =
    if (ints.size <= 1)
      Par_4.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par_4.map2(Par_4.fork(sum_4(l)), Par_4.fork(sum_4(r))) { _ + _ }
    }
  trait Par_4[A]
  object Par_4 {
    /** Run the given par in a parallel thread */
    def fork[A](p: => Par_4[A]): Par_4[A] = ???
    // Thanks to `fork' we can now make `unit' and `map2' strict,
    // leaving it up to the programmer to wrap arguments if they wish.
    def unit[A](f: => A): Par_4[A] = ???
    def map2[A,B,C](p1: => Par_4[A], p2: => Par_4[B])(f: (A,B) => C): Par_4[C] = ???
    // A non-strict version of unit can be implemented using `unit' and `fork'.
    def lazyUnit[A](f: => A): Par_4[A] = fork(unit(f))
  }

  // Last question now is "should evaluation of fork be eager or lazy?"
  // - If fork begins evaluating its argument immediately in parallel,
  //   this means we lose the ability to control the parallelism
  //   strategy used for different parts of our program.
  // - If fork fork simply holds on to its unevaluated argument until
  //   later, fork is more a description of a parallel computation that
  //   gets interpreted at a leter time by something kike the
  //   `Par_2.get' function.
  //
  // The first requires fork to access to the mechanism for
  // implementing parallelism, whereas The second doesn't. The second
  // implementation of fork just takes an unevaluated `Par' and
  // *marks* it for concurrent evaluation and it sounds better.
  //
  // If we choose the second meaning for fork, because fork is a
  // description of a parallel computation now `Par' is no more a
  // *container of a value*. But `Par' is more a *first-class program*
  // that we can *run*. So let's rename our get function to run and
  // dictate that this is where the parallelism actually gets
  // implemented: `def run[A](a: Par[A]): A'
}

object Par {
  // We know `run' needs to execute asynchronous tasks. We could write
  // our own low-level API but we prefere reuse the one in Java
  // Standard Library.
  // ExecutorService has a `submit' method that take a Callable value
  // (a lazy computation) and returns a Futur
  import java.util.concurrent.ExecutorService
  import java.util.concurrent.Future

  // Now `Par' that needs to execute asynchronous tasks, takes an
  // ExecutorService and return a Futur
  type Par[A] = ExecutorService => Future[A]

  /** Promotes a constatn value to a parallel computation */
  def unit[A](a: A): Par[A] = ???
  /** Combines the results of two parallel computation with a binary function */
  def map2[A,B,C](para: Par[A], parb: Par[B])(f: (A, B) => C): Par[C] = ???
  /** Marks a computation for concurrent evaluation. The evaluation
    * won't actually occur until forced by run */
  def fork[A](a: => Par[A]): Par[A] = ???
  /** Wraps its unevaluated argument in a Par and marks it for
    * concurrent evaluation */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  /** Extracts a value from a `Par' by actually performing the
    * computation */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
}

object FPInScalaParTest extends App {
}
