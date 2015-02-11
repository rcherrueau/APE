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

  // Let's see if we can avoid the aforementioned pitfall of combining
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
  // - If fork simply holds on to its unevaluated argument until
  //   later, fork is more a description of a parallel computation
  //   that gets interpreted at a later time by something like the
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
  // (a lazy computation) and returns a Future.
  import java.util.concurrent.ExecutorService
  import java.util.concurrent.Callable
  import java.util.concurrent.Future
  import java.util.concurrent.TimeUnit

  // Now `Par' -- that needs to execute asynchronous tasks -- takes an
  // ExecutorService and returns a Future.
  type Par[A] = ExecutorService => Future[A]

  /** Promotes a constatn value to a parallel computation */
  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  /** Future that just wraps a constant value. */
  private case class UnitFuture[A](get: A) extends Future[A] {
    def get(timeout: Long, units: TimeUnit): A = get
    def cancel(evenIfRunning: Boolean): Boolean = false
    def isCancelled = false
    def isDone = true
  }

  /** Combines the results of two parallel computation. */
  def map2[A,B,C](apar: Par[A], bpar: Par[B])(g: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af: Future[A] = apar(es)
      val bf: Future[B] = bpar(es)

      // FIXME: Respect the timeout of Future
      UnitFuture(g(af.get, bf.get))
    }

  // FIX to respect timeouts, we'd need a new `Future' implementation
  // that records the amount of time spent evaluating one future, then
  // subtracts that time from the available time allocated for
  // evaluating the other future.
  def map2_1[A,B,C](apar: Par[A], bpar: Par[B])(g: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af: Future[A] = apar(es)
      val bf: Future[B] = bpar(es)

      MapFuture(af, bf, g)
    }

  private case class MapFuture[A,B,C](af: Future[A],
                                      bf: Future[B],
                                      g: (A, B) => C) extends Future[C] {
    // We put the result of `get' in a cache value, thus if cache value
    // is Some, then the Future `isDone'.
    @volatile var cache: Option[C] = None

    def isCancelled = af.isCancelled || bf.isCancelled
    def isDone = cache.isDefined
    def get: C = cache match {
      case None => cache = Some(g(af.get, bf.get)); cache.get
      case Some(c) => c
    }
    def get(timeout: Long, units: TimeUnit): C = cache match {
      case None =>
        val ttl = TimeUnit.MILLISECONDS.convert(timeout, units)

        val start = System.currentTimeMillis
        val ares = af.get(ttl, TimeUnit.MILLISECONDS)
        val stop = System.currentTimeMillis; val atime = stop - start
        val bres = bf.get(ttl - atime, TimeUnit.MILLISECONDS)
        cache = Some(g(ares, bres))

        cache.get
      case Some(c) => c
    }
    def cancel(evenIfRunning: Boolean): Boolean =
      af.cancel(evenIfRunning) || bf.cancel(evenIfRunning)
  }


  /** Marks a computation for concurrent evaluation.
    *
    * The evaluation won't actually occur until forced by run
    */
  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) =>
    // It's only after the user calls `run' and the implementation
    // *reveive and ExecutorService* that we expose the Future
    // machinery.
    es.submit(new Callable[A] { def call = a(es).get })

  /** Wraps its unevaluated argument in a Par and marks it for
    * concurrent evaluation */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /** Extracts a value from a `Par' by actually performing the
    * computation. */
  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = {
    a(es)
  }

  /** Evaluates a function asynchronously. */
  def asyncF[A,B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  /** Sorts the list. */
  def sortPar(pl: Par[List[Int]]): Par[List[Int]] =
    map2(pl, unit(())) { (l,_) => l.sorted }

  /** Lifts a function */
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(())) { (a, _) => f(a) }

  // Sort using map
  def sortPar_2(pl: Par[List[Int]]): Par[List[Int]] =
    map(pl) { _.sorted }

  /** Combines a list of parallel */
  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    pas.foldRight[Par[List[A]]](unit(List[A]())) {
      (pa: Par[A], prest: Par[List[A]]) =>
      map2(pa, prest){ _ :: _ }
    }

  /** Maps over a list in parallel */
  // We've wrapped our implementaion in a call to `fork`. With this
  // implementation, `parMap` will return immediatly, even for a huge
  // input list. When we later call `run`, it will fork a single
  // asynchronous compuation which itself spawns N parallel
  // computations, and then waits for these computations to finish,
  // collecting their result into a list.
  def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] =
    sequence(as.map(asyncF(f)))

  /** Filters elements of a list in parallel */
  def parFilter[A](as: List[A])(p: A => Boolean): Par[List[A]] = {
    // First make a async filter for each item
    val asAsync: List[Par[Option[A]]] =
      as.map(asyncF((a:A) => if (p(a)) Some(a) else None))
    // Then wrap all this parallel filter.
    val asWrap: Par[List[Option[A]]] = sequence(asAsync)

    // Finaly, unboxe and export `Some' elements.
    map(asWrap)(for {
                  oa <- _ if oa.isDefined
                } yield oa.get)
  }

  // mapN in terms of map2
  def map3[A,B,C,D](apar: Par[A],
                    bpar: Par[B],
                    cpar: Par[C])(g: (A, B, C) => D): Par[D] = {
    val cdpar: Par[C => D] = map2(apar, bpar)(g.curried(_)(_))
    map2(cdpar, cpar)(_(_))
  }

  def map4[A,B,C,D,E](apar: Par[A],
                      bpar: Par[B],
                      cpar: Par[C],
                      dpar: Par[D])(g: (A, B, C, D) => E): Par[E] = {
    val cdepar: Par[C => D => E] = map2(apar, bpar)(g.curried(_)(_))
    val depar: Par[D => E] = map2(cdepar, cpar)(_(_))
    map2(depar, dpar)(_(_))
  }

  def map5[A,B,C,D,E,F](apar: Par[A],
                        bpar: Par[B],
                        cpar: Par[C],
                        dpar: Par[D],
                        epar: Par[E])(g: (A, B, C, D, E) => F): Par[F] = {
    val efpar: Par[E => F] = map4(apar, bpar, cpar, dpar)(
      g.curried(_)(_)(_)(_))
    map2(efpar, epar)(_(_))
  }

  /** Test that the result of two computation are equivalent. */
  def equal[A](es: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean = {
    p1(es).get == p2(es).get
  }

  // Refining combinators to their most general form
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (cond(es).get) t(es)
      else f(es)

  def choiceN[A](n: Par[Int])(l: List[Par[A]]): Par[A] =
    es => l(n(es).get)(es)

  def choice_2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(es => if (cond(es).get) unit(0)(es) else unit(1)(es))(
      List(t,f))

  def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => choices(key(es).get)(es)

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(pa(es).get)(es)

  // Using chooser
  def choice_3[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => chooser(cond)(if(_) t else f)(es)

  def choiceN_2[A](n: Par[Int])(l: List[Par[A]]): Par[A] =
    es => chooser(n)(l(_))(es)

  def choiceMap_2[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => chooser(key)(choices(_))(es)

  // `chooser` is a parallel computation that, when run, will run an
  // initial computation whose result is used to determine a second
  // computation. This function, which comes up often in functional
  // libraries, is usually called `flatMap`.
  def flatMap[A,B](apar: Par[A])(f: A => Par[B]): Par[B] =
    chooser(apar)(f)

  def join[A](p: Par[Par[A]]): Par[A] = es => (p(es).get())(es)

  def join_2[A](p: Par[Par[A]]): Par[A] =
    flatMap(p)(innerp => innerp)

  // Using join
  def flatMap_2[A,B](apar: Par[A])(f: A => Par[B]): Par[B] =
    join(map(apar)(f))

}

object FPInScalaParTest extends App {
  import Par.Par
  import java.util.concurrent.Executors

  val esCached = Executors.newCachedThreadPool
  val esT3 = Executors.newFixedThreadPool(3)

  Par.run(esT3)(
    Par.sequence(
      List(
        // One concurent computation
        Par.fork(Par.map(Par.unit(Thread sleep 1000)) {
                   _ => println("Sleep 1000")
                 }),
        // A second concurrent computation
        Par.fork(Par.map(Par.unit(Thread sleep 1)){
                   _ => println("Sleep 1")
                 }),
        // A third concurrent computation
        Par.fork(Par.map(Par.unit(Thread sleep 100)){
                   _ => println("Sleep 100")
                 }))))

  // Alternative syntax, without using `Par.run`
  Par.sequence(
    List(
      // One concurent computation
      Par.fork(Par.map(Par.unit(Thread sleep 1000)) {
                 _ => println("Sleep 1000")
               }),
      // A second concurrent computation
      Par.fork(Par.map(Par.unit(Thread sleep 1)){
                 _ => println("Sleep 1")
               }),
      // A third concurrent computation
      Par.fork(Par.map(Par.unit(Thread sleep 100)){
                 _ => println("Sleep 100")
               })))(esT3)

  /** Sums a list of int in parallel (see methodology) */
  def sum(ints: Seq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r))) { _ + _ }
    }

  Par.run(esCached)(
    Par.map(sum(List.range(1,100)))(println))

  println(
    Par.run(esCached)(
      Par.sequence(
        List(
          Par.choice(Par.lazyUnit(true))(
            Par.lazyUnit("true"),
            Par.lazyUnit("false")),
          Par.choice(Par.lazyUnit(false))(
            Par.lazyUnit("true"),
            Par.lazyUnit("false"))))).get)

  println(
    Par.run(esCached)(
      Par.sequence(
        List(
          Par.choice_2(Par.lazyUnit(true))(
            Par.lazyUnit("true"),
            Par.lazyUnit("false")),
          Par.choice_2(Par.lazyUnit(false))(
            Par.lazyUnit("true"),
            Par.lazyUnit("false"))))).get)

  println(
    Par.run(esCached)(
      Par.choiceMap(
        Par.lazyUnit("lala"))(
        Map("lala" -> Par.lazyUnit("lalalala")))).get)

  println(
    Par.run(esCached)(
      Par.chooser(Par.lazyUnit("live long"))(
        s => Par.map(Par.lazyUnit("and prosper")){ s + " " + _ })).get)

  println(
    Par.run(esCached)(
      Par.sequence(
        List(
          Par.choice_3(Par.lazyUnit(true))(
            Par.lazyUnit("true"),
            Par.lazyUnit("false")),
          Par.choice_3(Par.lazyUnit(false))(
            Par.lazyUnit("true"),
            Par.lazyUnit("false"))))).get)

  println(
    Par.run(esCached)(
      Par.choiceMap_2(
        Par.lazyUnit("lala"))(
        Map("lala" -> Par.lazyUnit("lalalala")))).get)

  val l = List.range(1,100)
  println(
    Par.run(esCached)(
      Par.flatMap(sum(l)) {
        r1 => Par.flatMap(sum(l)) {
          r2 => Par.map(sum(l)) {
            r3 => r1 + r2 + r3
          }
        }
      }).get)

  // shutdown the ExecutorService to doesn't accept new tasks
  esCached.shutdown
  esT3.shutdown

  // Breaking the law of forking: `fork(x) == x`
  /*
  val a: Par[Int] = Par.lazyUnit(41 + 1)
  println(Par.equal(Executors.newFixedThreadPool(1))(a, Par.fork(a)))
  // */
}
