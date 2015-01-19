package hcollection {
  // http://meta.plasm.us/posts/2013/06/09/learning-shapeless/
  // http://www.edofic.com/posts/2012-10-29-hlist-shapeless.html

  abstract class HList[+H,+T <: HList[_,_]] {
    def head: H
    def tail: T
    def ::[A](a: A): ::[A,HList[H,T]] = hcollection.::(a, this)
  }

  trait HNil extends HList[Nothing,Nothing] {
    def head = throw new IllegalAccessException("head of empty hlist")
    def tail = throw new IllegalAccessException("tail of empty hlist")
    // override of def required for good type inference (see test)
    override def ::[A](a: A): ::[A,HList[Nothing,Nothing]] = hcollection.::(a, this)
  }
  object HNil extends HNil

  case class ::[+H,+T <: HList[_,_]](val head: H,
                                     val tail: T) extends HList[H,T] {
    // override of def required for good type inference (see test)
    override def ::[A](a: A): ::[A,::[H,T]] = hcollection.::(a, this)
  }

  object HListTest {
    val myHList: Int :: String :: Double :: HList[Nothing,Nothing] =
      1 :: "hi" :: 2.0 :: HNil
  }
}

package dbhlist {
  trait DataBase {

  }

  trait DB[+H, +T <: DB[_,_]] extends DataBase {
    def head: Seq[H]
    def tail: T
    def |:[A](as: Seq[A]): |:[A,DB[H,T]] = dbhlist.|:(as, this)
  }

  trait EOCol extends DB[Nothing, Nothing] {
    def head = throw new IllegalAccessException("head of empty column")
    def tail = throw new IllegalAccessException("tail of empty column")
    // override of def required for good type inference (see test)
    override def |:[A](as: Seq[A]) = dbhlist.|:(as, this)
  }
  object EOCol extends EOCol

  case class |:[+H, +T <: DB[_,_]](val head: Seq[H],
                                   val tail: T) extends DB[H,T] {

    // override of def required for good type inference (see test)
    override def |:[A](as: Seq[A]) = dbhlist.|:(as, this)
  }

  import shapeless.Nat

  object DB {
    def apply[T1](ts: (T1)*): |:[T1,EOCol] = ts |: EOCol

    // Implicits are there to avoid apply ambiguity after type
    // erasure.
    def apply[T1,T2](ts: (T1, T2)*)(implicit
                                    $d01: DummyImplicit):
        |:[T1,|:[T2,EOCol]] = {
      val (t1s, t2s) = ts.foldLeft((Nil:List[T1],Nil:List[T2])) {
        case ((t1s, t2s), (t1, t2)) => (t1 :: t1s, t2 :: t2s)
      }

      t1s |: t2s |: EOCol
    }

    def apply[T1,T2,T3](ts: (T1,T2,T3)*)(implicit
                                         $d01: DummyImplicit,
                                         $d02: DummyImplicit):
        |:[T1,|:[T2,|:[T3,EOCol]]] = {
      val (t1s, t2s, t3s) =
        ts.foldLeft((Nil:List[T1],Nil:List[T2],Nil:List[T3])) {
          case ((t1s, t2s, t3s), (t1, t2, t3)) =>
            (t1 :: t1s, t2 :: t2s, t3 :: t3s)
        }

      t1s |: t2s |: t3s |: EOCol
    }

    def split[B <: DB[_,_],
              N <: Nat](db: B,
                        n: N)(
        implicit s: Split[B,N]) = {
      val indexes: List[Int] = List.range(0, db.head.size)
      val t: Tuple2[s.Prefix, s.Suffix] = s[N](db)

      val s1: Int|: s.Prefix = indexes |: t._1
    }
  }



  trait Split[D <: DB[_,_], N <: Nat] {
    type Prefix = DB[_,_]
    type Suffix = DB[_,_]
    type Out = (Prefix, Suffix)

    def apply[N](db: D): (Prefix, Suffix) = ???
  }

  object Split {
    implicit def split[D <: DB[_,_],
                       N <: Nat,
                       P <: DB[_,_],
                       S <: DB[_,_]]: Split[L,N] = new Split[L, N] {
      type Prefix = P
      type Suffix = S

    }
  }

  object DBTest {
    val myDB2: Int |: String |: EOCol = DB((0, "a"),
                                           (1, "b"),
                                           (2, "c"),
                                           (3, "d"))

    val myDB3: Int |: String |: Double |: EOCol = DB((0, "a", .0),
                                                     (1, "b", .1),
                                                     (2, "c", .2),
                                                     (3, "d", .3))



  }
}


package hlistshapeless {
  import shapeless._
  import poly._
  import ops.hlist.SplitLeft
  import ops.hlist.Split

  object DB {
    def apply[T1](ts: (T1)*) = ts :: HNil

    // Implicits are there to avoid apply ambiguity after type
    // erasure.
    def apply[T1,T2](ts: (T1, T2)*)(implicit
                                    $d01: DummyImplicit) = {
      val (t1s, t2s) = ts.foldLeft((Nil:List[T1],Nil:List[T2])) {
        case ((t1s, t2s), (t1, t2)) => (t1 :: t1s, t2 :: t2s)
      }

      t1s :: t2s :: HNil
    }

    def apply[T1,T2,T3](ts: (T1,T2,T3)*)(implicit
                                         $d01: DummyImplicit,
                                         $d02: DummyImplicit) = {
      val (t1s, t2s, t3s) =
        ts.foldLeft((Nil:List[T1],Nil:List[T2],Nil:List[T3])) {
          case ((t1s, t2s, t3s), (t1, t2, t3)) =>
            (t1 :: t1s, t2 :: t2s, t3 :: t3s)
        }

      t1s :: t2s :: t3s :: HNil
    }

    def split[L <: ::[Seq[_],_], N <: Nat](l: L, n: N)(implicit
                                                       s: Split[L,N]) = {
      l.split[N]
    }

    def indexes[L1 <: ::[Seq[_],_], L2 <: HList](t: (L1, L2)) = {
      val indexes = List.range(0, t._1.head.size)

      (indexes :: t._1, indexes :: t._2)
    }

  }


  object DBTest {
    val myDB2 = DB((0, "a"),
                   (1, "b"),
                   (2, "c"),
                   (3, "d"))

    val myDB3 = DB((0, "a", .0),
                   (1, "b", .1),
                   (2, "c", .2),
                   (3, "d", .3))

    val (split1, split2) = DB.indexes(DB.split(myDB3, Nat(2)))


    //  hlistshapeless.DBTest.split1
  }

}
