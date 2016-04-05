package <empty> {
  abstract trait TestState extends scala.AnyRef {
    def /*TestState*/$init$(): Unit = {
      ()
    };
    import scalaz._;
    import scalaz.Scalaz._;
    import _;
    import ops.hlist._;
    import Nat._;

    // https://wiki.scala-lang.org/display/SIW/Overview+of+Compiler+Phases

|    |-- def take[S <: HList] BYVALmode-EXPRmode (site: trait TestState)
|    |    |-- IndexedState[S, tk.Out, Unit] TYPEmode (site: method take in TestState)
|    |    |    |-- scalaz.`package` EXPRmode-POLYmode-QUALmode (site: method take in TestState)
|    |    |    |    \-> scalaz.type
|    |    |    |-- S TYPEmode (site: method take in TestState)
|    |    |    |    |--  <: HList TYPEmode (site: type S in TestState)
|    |    |    |    |    |-- HList TYPEmode (site: type S in TestState)
|    |    |    |    |    |    \-> shapeless.HList
|    |    |    |    |    [adapt]  <: shapeless.HList is now a TypeTree( <: shapeless.HList)
|    |    |    |    |    \->  <: shapeless.HList
|    |    |    |    \-> S
|    |    |    |-- tk.Out TYPEmode (site: method take in TestState)
|    |    |    |    |-- tk EXPRmode-POLYmode-QUALmode (site: method take in TestState)
|    |    |    |    |    |-- Take[S, n.N] TYPEmode (site: value tk in TestState)
|    |    |    |    |    |    |-- S TYPEmode (site: value tk in TestState)
|    |    |    |    |    |    |    \-> S
|    |    |    |    |    |    |-- n.N TYPEmode (site: value tk in TestState)
|    |    |    |    |    |    |    |-- n EXPRmode-POLYmode-QUALmode (site: value tk in TestState)
|    |    |    |    |    |    |    |    |-- Nat TYPEmode (site: value n in TestState)
|    |    |    |    |    |    |    |    |    \-> shapeless.Nat
|    |    |    |    |    |    |    |    \-> n.type (with underlying type shapeless.Nat)
|    |    |    |    |    |    |    \-> n.N
|    |    |    |    |    |    \-> shapeless.ops.hlist.Take[S,n.N]
|    |    |    |    |    \-> tk.type (with underlying type shapeless.ops.hlist.Take[S,n.N])
|    |    |    |    \-> tk.Out
|    |    |    |-- Unit TYPEmode (site: method take in TestState)
|    |    |    |    \-> Unit
|    |    |    \-> scalaz.IndexedState[S,tk.Out,Unit]
|    |    |--  <: HList TYPEmode (site: method take in TestState)
|    |    |    |-- HList TYPEmode (site: method take in TestState)
|    |    |    |    \-> shapeless.HList
|    |    |    [adapt]  <: shapeless.HList is now a TypeTree( <: shapeless.HList)
|    |    |    \->  <: shapeless.HList
|    |    |-- Nat TYPEmode (site: value n in TestState)
|    |    |    \-> shapeless.Nat
|    |    |-- Take[S, n.N] TYPEmode (site: value tk in TestState)
|    |    |    \-> shapeless.ops.hlist.Take[S,n.N]
|    |    |-- IndexedState[S, tk.Out, Unit] TYPEmode (site: method take in TestState)
|    |    |    |-- scalaz.`package` EXPRmode-POLYmode-QUALmode (site: method take in TestState)
|    |    |    |    \-> scalaz.type
|    |    |    |-- Unit TYPEmode (site: method take in TestState)
|    |    |    |    \-> Unit
|    |    |    \-> scalaz.IndexedState[S,tk.Out,Unit]
|    |    |-- $qmark$qmark$qmark : pt=scalaz.IndexedState[S,tk.Out,Unit] EXPRmode (site: method take in TestState)
|    |    |    \-> Nothing
|    |    \-> [def take] [S <: shapeless.HList](n: shapeless.Nat)(implicit tk: shapeless.ops.hlist.Take[S,n.N])scalaz.IndexedState[S,tk.Out,Unit]

    def take[S <: HList](n: Nat)(implicit tk: ops.hlist.Take[S,n.N]): scalaz.IndexedState[S,tk.Out,Unit] = scala.this.Predef.???;


|    |-- def v1 BYVALmode-EXPRmode (site: trait TestState)
|    |    |-- init[$colon$colon[Int, $colon$colon[String, HNil]]].flatM... EXPRmode (site: method v1 in TestState)
|    |    |    |-- init[$colon$colon[Int, $colon$colon[String, HNil]]].flatM... BYVALmode-EXPRmode-FUNmode-POLYmode (silent: method v1 in TestSta te)
|    |    |    |    |-- init[$colon$colon[Int, $colon$colon[String, HNil]]].flatM... EXPRmode-POLYmode-QUALmode (silent: method v1 in TestState)
|    |    |    |    |    |-- init[$colon$colon[Int, $colon$colon[String, HNil]]].flatMap BYVALmode-EXPRmode-FUNmode-POLYmode (silent: method v1 in TestState)
|    |    |    |    |    |    |-- init[$colon$colon[Int, $colon$colon[String, HNil]]] EXPRmode-POLYmode-QUALmode (silent: method v1 in TestState)
|    |    |    |    |    |    |    |-- init BYVALmode-EXPRmode-FUNmode-POLYmode-TAPPmode (silent: method v1 in TestState)
|    |    |    |    |    |    |    |    \-> [S]=> scalaz.State[S,S]
|    |    |    |    |    |    |    |-- $colon$colon[Int, $colon$colon[String, HNil]] TYPEmode (silent: method v1 in TestState)
|    |    |    |    |    |    |    |    |-- Int TYPEmode (silent: method v1 in TestState)
|    |    |    |    |    |    |    |    |    \-> Int
|    |    |    |    |    |    |    |    |-- $colon$colon[String, HNil] TYPEmode (silent: method v1 in TestState)
|    |    |    |    |    |    |    |    |    |-- String TYPEmode (silent: method v1 in TestState)
|    |    |    |    |    |    |    |    |    |    [adapt] String is now a TypeTree(String)
|    |    |    |    |    |    |    |    |    |    \-> String
|    |    |    |    |    |    |    |    |    |-- HNil TYPEmode (silent: method v1 in TestState)
|    |    |    |    |    |    |    |    |    |    \-> shapeless.HNil
|    |    |    |    |    |    |    |    |    \-> shapeless.::[String,shapeless.HNil]
|    |    |    |    |    |    |    |    \-> shapeless.::[Int,shapeless.::[String,shapeless.HNil]]
|    |    |    |    |    |    |    \-> scalaz.State[shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.::[Int,shapeless.::[String,shapeless.HNil]]]
|    |    |    |    |    |    [adapt] [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit... adapted to [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit...
|    |    |    |    |    |    \-> (f: shapeless.::[Int,shapeless.::[String,shapeless.HNil]] => scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],S3,B])(implicit F: scalaz.Bind[scalaz.Id.Id])scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],S3,B]
|    |    |    |    |    |-- ((x$1) => take(_1)) : pt=shapeless.::[Int,shapeless.::[String,shapeless.HNil]] => scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],?,?] BYVALmode-EXPRmode-POLYmode (silent: method v1 in TestState)
|    |    |    |    |    |    |-- take(_1) : pt=scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],?,?] EXPRmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |-- take BYVALmode-EXPRmode-FUNmode-POLYmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |    [adapt] [S <: shapeless.HList](n: shapeless.Nat)(implicit tk: sha... adapted to [S <: shapeless.HList](n: shapeless.Nat)(implicit tk: sha...
|    |    |    |    |    |    |    |    \-> (n: shapeless.Nat)(implicit tk: shapeless.ops.hlist.Take[S,n.N])scalaz.IndexedState[S,tk.Out,Unit]
|    |    |    |    |    |    |    |-- _1 : pt=shapeless.Nat BYVALmode-EXPRmode-POLYmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |    \-> shapeless.Nat._1
|    |    |    |    |    |    |    solving for (S: ?S)
|    |    |    |    |    |    |    solving for (H: ?H, T: ?T, N: ?N, Out: ?Out)
|    |    |    |    |    |    |    solving for (L: ?L)
|    |    |    |    |    |    |    [adapt] [L <: shapeless.HList]=> shapeless.ops.hlist.Take.Aux[L,s... adapted to [L <: shapeless.HList]=> shapeless.ops.hlist.Take.Aux[L,s... based on pt shapeless.ops.hlist.Take[shapeless.::[String,shapeless.HNil],shapeless._0]
|    |    |    |    |    |    |    solving for (Out: ?Out)
|    |    |    |    |    |    |    |-- [H, T <: shapeless.HList, N <: shapeless.Nat, Out <: shap... : pt=shapeless.ops.hlist.Take[shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.Nat._1.N] EXPRmode (silent solving: type Out: value $anonfun in TestState) implicits disabled
|    |    |    |    |    |    |    |    solving for (Out: ?Out)
|    |    |    |    |    |    |    |    \-> shapeless.ops.hlist.Take[shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.Succ[shapeless._0]]{type Out = shapeless.::[Int,shapeless.HNil]}
|    |    |    |    |    |    |    [adapt] [H, T <: shapeless.HList, N <: shapeless.Nat, Out <: shap... adapted to [H, T <: shapeless.HList, N <: shapeless.Nat, Out <: shap... based on pt shapeless.ops.hlist.Take[shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.Nat._1.N]
|    |    |    |    |    |    |    |-- [S <: shapeless.HList](n: shapeless.Nat)(implicit tk: sha... : pt=scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],?,?] EXPRmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |    \-> scalaz.IndexedStateT[[+X]X,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.::[Int,shapeless.HNil],Unit]
|    |    |    |    |    |    |    [adapt] [S <: shapeless.HList](n: shapeless.Nat)(implicit tk: sha... adapted to [S <: shapeless.HList](n: shapeless.Nat)(implicit tk: sha... based on pt scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],?,?]
|    |    |    |    |    |    |    \-> scalaz.IndexedStateT[[+X]X,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.::[Int,shapeless.HNil],Unit]
|    |    |    |    |    |    \-> shapeless.::[Int,shapeless.::[String,shapeless.HNil]] => scalaz.IndexedStateT[[+X]X,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.::[Int,shapeless.HNil],Unit]
|    |    |    |    |    solving for (S3: ?S3, B: ?B)
|    |    |    |    |    |-- scalaz.`package` EXPRmode-POLYmode-QUALmode (silent: method v1 in TestState) implicits disabled
|    |    |    |    |    |    \-> scalaz.type
|    |    |    |    |    [adapt] idInstance adapted to => scalaz.Traverse1[scalaz.Id.Id] with scalaz.Each[scalaz... based on pt scalaz.Bind[scalaz.Id.Id]
|    |    |    |    |    |-- [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit... EXPRmode-POLYmode-QUALmode (silent: method v1 in TestState)
|    |    |    |    |    |    \-> scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.::[Int,shapeless.HNil],Unit]
|    |    |    |    |    [adapt] [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit... adapted to [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit...
|    |    |    |    |    \-> scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.::[Int,shapeless.HNil],Unit]
|    |    |    |    [adapt] [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit... adapted to [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit...
|    |    |    |    \-> (f: Unit => scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.HNil],S3,B])(implicit F: scalaz.Bind[scalaz.Id.Id])scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],S3,B]
|    |    |    |-- ((_) => state("abc").map(((x) => x))) : pt=Unit => scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.HNil],?,?] BYVALmode-EXPRmode-POLYmode (site: method v1 in TestState)
|    |    |    |    |-- state("abc").map(((x) => x)) : pt=scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.HNil],?,?] EXPRmode (site: value $anonfun in TestState)
|    |    |    |    |    |-- state("abc").map BYVALmode-EXPRmode-FUNmode-POLYmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |-- state("abc") EXPRmode-POLYmode-QUALmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |-- state BYVALmode-EXPRmode-FUNmode-POLYmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |    [adapt] [S, A](a: A)scalaz.State[S,A] adapted to [S, A](a: A)scalaz.State[S,A]
|    |    |    |    |    |    |    |    \-> (a: A)scalaz.State[S,A]
|    |    |    |    |    |    |    |-- "abc" BYVALmode-EXPRmode-POLYmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |    \-> String("abc")
|    |    |    |    |    |    |    solving for (S: ?S, A: ?A)
|    |    |    |    |    |    |    \-> scalaz.State[S,String]
|    |    |    |    |    |    [adapt] [B](f: A => B)(implicit F: scalaz.Functor[F])scalaz.Index... adapted to [B](f: A => B)(implicit F: scalaz.Functor[F])scalaz.Index...
|    |    |    |    |    |    \-> (f: String => B)(implicit F: scalaz.Functor[scalaz.Id.Id])scalaz.IndexedStateT[scalaz.Id.Id,S,S,B]
|    |    |    |    |    |-- ((x) => x) : pt=String => ? BYVALmode-EXPRmode-POLYmode (site: value $anonfun in TestState)
|    |    |    |    |    |    |-- x EXPRmode (site: value $anonfun in TestState)
|    |    |    |    |    |    |    \-> String
|    |    |    |    |    |    \-> String => String
|    |    |    |    |    solving for (S: ?S, B: ?B)
|    |    |    |    |    |-- scalaz.`package` EXPRmode-POLYmode-QUALmode (silent: value $anonfun in TestState) implicits disabled
|    |    |    |    |    |    \-> scalaz.type
|    |    |    |    |    [adapt] idInstance adapted to => scalaz.Traverse1[scalaz.Id.Id] with scalaz.Each[scalaz... based on pt scalaz.Functor[scalaz.Id.Id]
|    |    |    |    |    |-- [B](f: A => B)(implicit F: scalaz.Functor[F])scalaz.Index... : pt=scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.HNil],?,?] EXPRmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    \-> scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.HNil],shapeless.::[Int,shapeless.HNil],String]
|    |    |    |    |    [adapt] [B](f: A => B)(implicit F: scalaz.Functor[F])scalaz.Index... adapted to [B](f: A => B)(implicit F: scalaz.Functor[F])scalaz.Index... based on pt scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.HNil],?,?]
|    |    |    |    |    \-> scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.HNil],shapeless.::[Int,shapeless.HNil],String]
|    |    |    |    \-> Unit => scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.HNil],shapeless.::[Int,shapeless.HNil],String]
|    |    |    solving for (S3: ?S3, B: ?B)
|    |    |    |-- scalaz.`package` EXPRmode-POLYmode-QUALmode (silent: method v1 in TestState) implicits disabled
|    |    |    |    \-> scalaz.type
|    |    |    [adapt] idInstance adapted to => scalaz.Traverse1[scalaz.Id.Id] with scalaz.Each[scalaz... based on pt scalaz.Bind[scalaz.Id.Id]
|    |    |    |-- [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit... EXPRmode (site: method v1 in TestState)
|    |    |    |    \-> scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.::[Int,shapeless.HNil],String]
|    |    |    [adapt] [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit... adapted to [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit...
|    |    |    \-> scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.::[Int,shapeless.HNil],String]
|    |    \-> [def v1] => scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.::[Int,shapeless.HNil],String]

    def v1: scalaz.IndexedStateT[scalaz.Id.Id,::[Int,::[String,HNil]],::[Int,HNil],String] = scalaz.Scalaz.init[::[Int,::[String,HNil]]].flatMap[::[Int,HNil], Unit](((x$1: ::[Int,::[String,HNil]]) => TestState.this.take[::[Int,::[String,HNil]]](Nat._1)(hlist.this.Take.hlistTake2[Int, ::[String,HNil], _0, Nothing](hlist.this.Take.hlistTake1[::[String,HNil]]))))(scalaz.`package`.idInstance).flatMap[::[Int,HNil], String](((_: Unit) => scalaz.Scalaz.state[::[Int,HNil], String]("abc").map[String](((x: String) => x))(scalaz.`package`.idInstance)))(scalaz.`package`.idInstance);


|    |-- def v2 BYVALmode-EXPRmode (site: trait TestState)
|    |    |-- init[$colon$colon[Int, $colon$colon[String, HNil]]].flatM... EXPRmode (site: method v2 in TestState)
|    |    |    |-- init[$colon$colon[Int, $colon$colon[String, HNil]]].flatMap BYVALmode-EXPRmode-FUNmode-POLYmode (silent: method v2 in TestState)
|    |    |    |    |-- init[$colon$colon[Int, $colon$colon[String, HNil]]] EXPRmode-POLYmode-QUALmode (silent: method v2 in TestState)
|    |    |    |    |    |-- init BYVALmode-EXPRmode-FUNmode-POLYmode-TAPPmode (silent: method v2 in TestState)
|    |    |    |    |    |    \-> [S]=> scalaz.State[S,S]
|    |    |    |    |    |-- $colon$colon[Int, $colon$colon[String, HNil]] TYPEmode (silent: method v2 in TestState)
|    |    |    |    |    |    |-- Int TYPEmode (silent: method v2 in TestState)
|    |    |    |    |    |    |    \-> Int
|    |    |    |    |    |    |-- $colon$colon[String, HNil] TYPEmode (silent: method v2 in TestState)
|    |    |    |    |    |    |    |-- String TYPEmode (silent: method v2 in TestState)
|    |    |    |    |    |    |    |    [adapt] String is now a TypeTree(String)
|    |    |    |    |    |    |    |    \-> String
|    |    |    |    |    |    |    |-- HNil TYPEmode (silent: method v2 in TestState)
|    |    |    |    |    |    |    |    \-> shapeless.HNil
|    |    |    |    |    |    |    \-> shapeless.::[String,shapeless.HNil]
|    |    |    |    |    |    \-> shapeless.::[Int,shapeless.::[String,shapeless.HNil]]
|    |    |    |    |    \-> scalaz.State[shapeless.::[Int,shapeless.::[String,shapeless.HNil]],shapeless.::[Int,shapeless.::[String,shapeless.HNil]]]
|    |    |    |    [adapt] [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit... adapted to [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit...
|    |    |    |    \-> (f: shapeless.::[Int,shapeless.::[String,shapeless.HNil]] => scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],S3,B])(implicit F: scalaz.Bind[scalaz.Id.Id])scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],S3,B]
|    |    |    |-- ((x$2) => take(_1).flatMap(((x$3) => state("abc").map(((x... : pt=shapeless.::[Int,shapeless.::[String,shapeless.HNil]] => scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],?,?] BYVALmode-EXPRmode-POLYmode (site: method v2 in TestState)
|    |    |    |    |-- take(_1).flatMap(((x$3) => state("abc").map(((x) => x)))) : pt=scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],?,?] EXPRmode (site: value $anonfun in TestState)
|    |    |    |    |    |-- take(_1).flatMap BYVALmode-EXPRmode-FUNmode-POLYmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |-- take(_1) EXPRmode-POLYmode-QUALmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |-- take BYVALmode-EXPRmode-FUNmode-POLYmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |    [adapt] [S <: shapeless.HList](n: shapeless.Nat)(implicit tk: sha... adapted to [S <: shapeless.HList](n: shapeless.Nat)(implicit tk: sha...
|    |    |    |    |    |    |    |    \-> (n: shapeless.Nat)(implicit tk: shapeless.ops.hlist.Take[S,n.N])scalaz.IndexedState[S,tk.Out,Unit]
|    |    |    |    |    |    |    |-- _1 : pt=shapeless.Nat BYVALmode-EXPRmode-POLYmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |    \-> shapeless.Nat._1
|    |    |    |    |    |    |    solving for (S: ?S)
|    |    |    |    |    |    |    solving for (S: ?S)
|    |    |    |    |    |    |    solving for (H: ?H, T: ?T, N: ?N, Out: ?Out)
|    |    |    |    |    |    |    solving for (L: ?L)
|    |    |    |    |    |    |    [adapt] [L <: shapeless.HList]=> shapeless.ops.hlist.Take.Aux[L,s... adapted to [L <: shapeless.HList]=> shapeless.ops.hlist.Take.Aux[L,s... based on pt shapeless.ops.hlist.Take[T,shapeless._0]
|    |    |    |    |    |    |    solving for (H: ?H, T: ?T, Out: ?Out)
|    |    |    |    |    |    |    |-- [H, T <: shapeless.HList, N <: shapeless.Nat, Out <: shap... : pt=shapeless.ops.hlist.Take[?,shapeless.Nat._1.N] EXPRmode (silent solving: type H,type T,type Out: value $anonfun in TestState) implicits disabled
|    |    |    |    |    |    |    |    solving for (H: ?H, T: ?T, Out: ?Out)
|    |    |    |    |    |    |    |    solving for (H: ?H, T: ?T)
|    |    |    |    |    |    |    |    \-> shapeless.ops.hlist.Take[shapeless.::[Nothing,Nothing],shapeless.Succ[shapeless._0]]{type Out = shapeless.::[Nothing,tt.Out]}
|    |    |    |    |    |    |    [adapt] [H, T <: shapeless.HList, N <: shapeless.Nat, Out <: shap... adapted to [H, T <: shapeless.HList, N <: shapeless.Nat, Out <: shap... based on pt shapeless.ops.hlist.Take[S,shapeless.Nat._1.N]
|    |    |    |    |    |    |    |-- [S <: shapeless.HList](n: shapeless.Nat)(implicit tk: sha... EXPRmode-POLYmode-QUALmode (silent solving: type S: value $anonfun in TestState)
|    |    |    |    |    |    |    |    solving for (S: ?S)
|    |    |    |    |    |    |    |    \-> <error>
|    |    |    |    |    |    |    [adapt] [S <: shapeless.HList](n: shapeless.Nat)(implicit tk: sha... adapted to [S <: shapeless.HList](n: shapeless.Nat)(implicit tk: sha...
|    |    |    |    |    |    |    \-> <error>
|    |    |    |    |    |    \-> <error>
[error] /home/rl3x-desktop/prog/APE/scala/type/State.scala:18: could not find implicit value for parameter tk: shapeless.ops.hlist.Take[S,shapeless.Nat._1.N]
[error]     take(_1)                    flatMap { _ =>
[error]         ^
|    |    |    |    |    |-- ((x$3) => state("abc").map(((x) => x))) : pt=<error> EXPRmode (site solving: type S: value $anonfun in TestState)
|    |    |    |    |    |    |-- state("abc").map(((x) => x)) EXPRmode (site: value $anonfun in TestState)
|    |    |    |    |    |    |    |-- state("abc").map BYVALmode-EXPRmode-FUNmode-POLYmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |    |-- state("abc") EXPRmode-POLYmode-QUALmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |    |    |-- state BYVALmode-EXPRmode-FUNmode-POLYmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |    |    |    [adapt] [S, A](a: A)scalaz.State[S,A] adapted to [S, A](a: A)scalaz.State[S,A]
|    |    |    |    |    |    |    |    |    |    \-> (a: A)scalaz.State[S,A]
|    |    |    |    |    |    |    |    |    |-- "abc" BYVALmode-EXPRmode-POLYmode (silent: value $anonfun in TestState)
|    |    |    |    |    |    |    |    |    |    \-> String("abc")
|    |    |    |    |    |    |    |    |    solving for (S: ?S, A: ?A)
|    |    |    |    |    |    |    |    |    \-> scalaz.State[S,String]
|    |    |    |    |    |    |    |    [adapt] [B](f: A => B)(implicit F: scalaz.Functor[F])scalaz.Index... adapted to [B](f: A => B)(implicit F: scalaz.Functor[F])scalaz.Index...
|    |    |    |    |    |    |    |    \-> (f: String => B)(implicit F: scalaz.Functor[scalaz.Id.Id])scalaz.IndexedStateT[scalaz.Id.Id,S,S,B]
|    |    |    |    |    |    |    |-- ((x) => x) : pt=String => ? BYVALmode-EXPRmode-POLYmode (site: value $anonfun in TestState)
|    |    |    |    |    |    |    |    |-- x EXPRmode (site: value $anonfun in TestState)
|    |    |    |    |    |    |    |    |    \-> String
|    |    |    |    |    |    |    |    \-> String => String
|    |    |    |    |    |    |    solving for (S: ?S, B: ?B)
|    |    |    |    |    |    |    solving for (S: ?S)
|    |    |    |    |    |    |    |-- scalaz.`package` EXPRmode-POLYmode-QUALmode (silent: value $anonfun in TestState) implicits disabled
|    |    |    |    |    |    |    |    \-> scalaz.type
|    |    |    |    |    |    |    [adapt] idInstance adapted to => scalaz.Traverse1[scalaz.Id.Id] with scalaz.Each[scalaz... based on pt scalaz.Functor[scalaz.Id.Id]
|    |    |    |    |    |    |    solving for (S: ?S)
|    |    |    |    |    |    |    |-- [B](f: A => B)(implicit F: scalaz.Functor[F])scalaz.Index... EXPRmode (site solving: type S: value $anonfun in TestState)
|    |    |    |    |    |    |    |    solving for (S: ?S)
|    |    |    |    |    |    |    |    solving for (S: ?S)
|    |    |    |    |    |    |    |    \-> scalaz.IndexedStateT[scalaz.Id.Id,Nothing,Nothing,String]
|    |    |    |    |    |    |    [adapt] [B](f: A => B)(implicit F: scalaz.Functor[F])scalaz.Index... adapted to [B](f: A => B)(implicit F: scalaz.Functor[F])scalaz.Index...
|    |    |    |    |    |    |    \-> scalaz.IndexedStateT[scalaz.Id.Id,Nothing,Nothing,String]
|    |    |    |    |    |    solving for (S: ?S)
|    |    |    |    |    |    \-> <error> => scalaz.IndexedStateT[scalaz.Id.Id,Nothing,Nothing,String]
|    |    |    |    |    \-> <error>
|    |    |    |    \-> shapeless.::[Int,shapeless.::[String,shapeless.HNil]] => <error>
|    |    |    solving for (S3: ?S3, B: ?B)
|    |    |    solving for (S3: ?S3, B: ?B)
|    |    |    |-- scalaz.`package` EXPRmode-POLYmode-QUALmode (silent: method v2 in TestState) implicits disabled
|    |    |    |    \-> scalaz.type
|    |    |    [adapt] idInstance adapted to => scalaz.Traverse1[scalaz.Id.Id] with scalaz.Each[scalaz... based on pt scalaz.Bind[scalaz.Id.Id]
|    |    |    solving for (S3: ?S3, B: ?B)
|    |    |    |-- [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit... EXPRmode (site solving: type S3,type B: method v2 in TestState)
|    |    |    |    solving for (S3: ?S3, B: ?B)
|    |    |    |    solving for (S3: ?S3, B: ?B)
|    |    |    |    \-> scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],Nothing,Nothing]
|    |    |    [adapt] [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit... adapted to [S3, B](f: A => scalaz.IndexedStateT[F,S2,S3,B])(implicit...
|    |    |    \-> scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],Nothing,Nothing]
|    |    \-> [def v2] => scalaz.IndexedStateT[scalaz.Id.Id,shapeless.::[Int,shapeless.::[String,shapeless.HNil]],Nothing,Nothing]
    def v2: scalaz.IndexedStateT[scalaz.Id.Id,::[Int,::[String,HNil]],Nothing,Nothing] = scalaz.Scalaz.init[::[Int,::[String,HNil]]].flatMap[Nothing, Nothing](((x$2: ::[Int,::[String,HNil]]) => take(_1).flatMap(((<x$3: error>: <error>) => state("abc").map(((x: String) => x))))))(scalaz.`package`.idInstance)

  }
}
