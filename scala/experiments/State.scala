trait TestState {
  // import scalaz._, Scalaz._
  // import shapeless._,  ops.hlist._, Nat._

  // Réécrire l'exemple avec type class Order et une monade plus
  // simple

  // def take[S <: HList](n: Nat)(implicit tk: Take[S, n.N]):
  //     IndexedState[S, tk.Out, Unit] = ???

  // def v1 =
  //   init[Int :: String :: HNil] flatMap { _ =>
  //   take(_1)                    flatMap { _ =>
  //   state("abc")                map     { x => x } } }

  // def v2 = for {
  //   _ <- init[Int :: String :: HNil]
  //   _ <- take(_1)
  //   x <- state("abc")
  // } yield x

  // def v3 = for {
  //     _ <- init[Int :: String :: HNil] flatMap { _ => take(_1) }
  //     x <- state("abc")
  //   } yield x

  // def take[S <: HList](hl: S, n: Nat)(implicit tk: Take[S, n.N]):
  //     IndexedState[S, tk.Out, Unit] = ???

  // for {
  //   hl <- init[Int :: String :: HNil]
  //   _  <- take(hl, _1)
  //   _  <- take[Int :: HNil](_1)
  //   x  <- state("abc")
  // } yield x

  // Type class with dependent type
  trait TC[T] { type Out }
  implicit val tcInt: TC[Int] = new TC[Int] { type Out = Int }

  case class M[S1,S2]() {
    def flatMap[S3](f: Unit => M[S2,S3]): M[S2,S3] = ???
    def map(f: Unit => Unit): M[S1,S2] = ???
  }

  object M {
    def unit[S]: M[S,S] = ???

    // Uses the type class and dependent type.
    def useTC[S](implicit tc: TC[S]): M[S,tc.Out] = ???

    // unit[Int]    flatMap { _ =>
    // useTC        flatMap { _ =>
    // unit         map     { _ => () } } }

    // for {
    //   _ <- unit[Int]
    //   _ <- useTC
    //   _ <- unit
    // } yield ()

    for {
      _ <- unit[Int] flatMap { _ => useTC }
      _ <- unit
    } yield ()
  }

}
