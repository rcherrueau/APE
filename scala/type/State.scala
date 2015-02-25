trait TestState {
  import scalaz._, Scalaz._
  import shapeless._,  ops.hlist._, Nat._

  def take[S <: HList](n: Nat)(implicit tk: Take[S, n.N]):
      IndexedState[S, tk.Out, Unit] = ???

  // v1
  def v1 =
    for {
      _ <- init[Int :: String :: HNil] flatMap { _ => take(_1) }
      x <- state("abc")
    } yield x

  // v2
  def v2 =
    init[Int :: String :: HNil] flatMap { _ =>
    take(_1)                    flatMap { _ =>
    state("abc")                map     { x => x } } }

  // // v3
  // for {
  //   _ <- init[Int :: String :: HNil]
  //   _ <- take(_1)
  //   x <- state("abc")
  // } yield x


  // def take[S <: HList](hl: S, n: Nat)(implicit tk: Take[S, n.N]):
  //     IndexedState[S, tk.Out, Unit] = ???

  // for {
  //   hl <- init[Int :: String :: HNil]
  //   _  <- take(hl, _1)
  //   _  <- take[Int :: HNil](_1)
  //   x  <- state("abc")
  // } yield x
}
