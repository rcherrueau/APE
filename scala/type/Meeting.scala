import spire.algebra._
import spire.implicits._
import utils._

// [[http://eed3si9n.com/learning-scalaz/]]

//--------------------------------------------------------- Cypher Library
trait CypherType {
  // Types
  sealed trait Cypher[F] { def data: F }
  sealed abstract class CypherEq[CC[X] <: Cypher[X], F: Eq]
      extends Cypher[F] with Eq[CC[F]] {
    def F: Eq[F] = implicitly[Eq[F]]
    override def eqv(x: CC[F], y: CC[F]) = F.eqv(x.data,y.data)
  }
  sealed abstract class CypherOrd[CC[X] <: Cypher[X], F: Order]
      extends Cypher[F] with Order[CC[F]] {
    def F: Order[F] = implicitly[Order[F]]
    override def compare(x: CC[F], y: CC[F]) = F.compare(x.data, y.data)
  }

  // Type alias
  type CEq[CC[X] <: CypherType#Cypher[X]] = { type λ[F] = Eq[CC[F]] }
  type COrder[CC[X] <: CypherType#Cypher[X]] = { type λ[F] = Order[CC[F]] }
}

// Homomorphic Encryption Scheme
trait Hes extends CypherType {
  // Types
  sealed trait Hes[A] extends Cypher[A]
  sealed class HesEq[F: Eq] (val data: F = ???) extends CypherEq[HesEq, F]
  sealed class HesOrd[F: Order] (val data: F = ???) extends CypherOrd[HesOrd, F]

  // Eq and Order augmentation
  implicit def heseq[F: Eq]: Eq[HesEq[F]] = new HesEq[F]()
  implicit def hesord[F: Order]: Order[HesOrd[F]] = new HesOrd[F]()

  // Factory
  object HesEnc {
    def withEq[F: Eq](data: F): HesEq[F] = new HesEq(data)
    def withOrd[F: Order](data: F): HesOrd[F] = new HesOrd(data)
  }
}

// Advanced Encryption Stantard
trait Aes extends CypherType {
  // Types
  sealed trait Aes[F] extends Cypher[F]
  case class AesEnc[F](val data: F) extends Aes[F]
}

object cypher extends CypherType with Hes with Aes

object MeetingsApp extends App {
  import cypher._
  import utils._

  object utils {
    trait Stats {
      def count[A: Eq](l: List[A]): List[(A, Int)] = l match {
        case Nil => Nil
        case hd :: tl =>
          (hd, 1 + tl.filter(hd === _).length) ::
          count(tl.filter(hd =!= _))
      }
    }

    def meeting(date: String, name: String, address: String) =
      (date, name, address)
  }

  //------------------------------------------------------------- Services
  object Calendar {
   // meetings^{D: Order, N: Eq}
    def meetings[D: Order, N: Eq, A](ts: List[(D,N,A)],
                                     date: D,
                                     name: N): List[(D,N,A)] =
      for ((d,n,a) <- ts
        if n === name;
        if d >= date) yield (d,n,a)
  }

  object Stats1 extends Stats {
    // mostVisitedClient^{N: Eq}
    def mostVisitedClient[N: Eq](ts: List[(_,N,_)]): N =
      count(ts.map(_._2)).maxBy(_._2)._1

    // mostBusyDay^{D: Eq, N: Raw}
    def mostBusyDay[D: Eq, N](ts: List[(D,N,_)]): (D, List[N]) = {
      val mostBusyDay = count(ts.map(_._1)).maxBy(_._2)._1
      (mostBusyDay, ts.filter(_._1 === mostBusyDay).map(_._2))
    }
  }

  object Stats2 extends Stats {
    // mostVisitedClient^{A: Eq}
    def visitedPlaces[A: Eq](ts: List[(_,_,A)]): List[(A, Int)] =
      count(ts.map(_._3))
  }

  //---------------------------------------------------------------- Datas
  val ts =
    meeting("2014-01-01", "Bob",   "a") ::
    meeting("2014-01-02", "Chuck", "b") ::
    meeting("2014-01-03", "Bob",   "c") ::
    meeting("2014-01-04", "Chuck", "d") ::
    meeting("2014-01-05", "Bob",   "e") ::
    meeting("2014-01-06", "Bob",   "e") ::
    meeting("2014-01-07", "Bob",   "e") ::
    meeting("2014-01-08", "Bob",   "f") ::
    meeting("2014-01-09", "Chuck", "b") ::
    meeting("2014-01-10", "Chuck", "g") :: Nil

  val date = "2014-01-06"

  val name = "Bob"

  //---------------------------------------------------------------- Tests
  // val f: (List[(D,N,A)], D, N) => List[(D,N,A)] = (Calendar.meetings _)
  // val fcurry: List[(D,N,A)] => D => N => List[(D,N,A)] = f.curried
  // val g: List[(D,N,_)] => (D, List[N]) = (Stats1.mostBusyDay _)
  // val h: N => (D, List[N]) = g compose (fcurry(ts)(date))
  // applyf(implicitly[Order[String]],implicitly[Eq[String]])(("a","a","a") :: Nil)("a")("a")
  object App1 {
    // (mostBusyDay • meetings)
    def apply[D: Order, N: Eq, A](ts: List[(D,N,A)],
                                  date: D,
                                  name: N): (D, List[N]) =
      Stats1.mostBusyDay(Calendar.meetings(ts, date, name))
    // Following is the code that return the function
    def applyf[D: Order, N: Eq, A]: List[(D,N,A)] => D => N => (D, List[N]) =
      (apply[D,N,A] _).curried

    // Enc^{D: ord, N: eq}(mostBusyDay • meetings)
    //
    // It means that D should be CypherOrd[D] and N should be
    // CypherEq[N]. There is two ways to achieved this. First with
    // constraint on subtyping relation and type classes.
    def v1[CD[X] <: Cypher[X],
           CN[X] <: Cypher[X],
           D, N, A] (ts: List[(CD[D],CN[N],A)],
                     date: CD[D],
                     name: CN[N])
                    (implicit $ev1: Order[CD[D]],
                              $ev2: Eq[CN[N]]): (CD[D], List[CN[N]]) =
      apply(ts, date, name)
    // Following is the code that return the function
    def v1f[CD[X] <: Cypher[X],
            CN[X] <: Cypher[X],
            D, N, A](implicit $ev1: Order[CD[D]],
                              $ev2: Eq[CN[N]]):
        List[(CD[D],CN[N],A)] => CD[D] => CN[N] => (CD[D], List[CN[N]]) =
      applyf[CD[D], CN[N], A]
    // Same as v1 but with context bound.
    def v2[CD[X] <: Cypher[X],
           CN[X] <: Cypher[X],
           D: COrder[CD]#λ,
           N: CEq[CN]#λ,
           A] (ts: List[(CD[D],CN[N],A)],
                     date: CD[D],
                     name: CN[N]): (CD[D], List[CN[N]]) =
      apply(ts, date, name)
    // Same as v1f but with context bound.
    def v2f[CD[X] <: Cypher[X],
            CN[X] <: Cypher[X],
            D: COrder[CD]#λ,
            N: CEq[CN]#λ,
            A]:
        List[(CD[D],CN[N],A)] => CD[D] => CN[N] => (CD[D], List[CN[N]]) =
      applyf[CD[D],CN[N],A]
    // //
    // // Second with combinators.
    // // TODO: Set combinators as arguments of the function.
    // def v3[CD[X] <: Cypher[X] with Order[CD[D]],
    //        CN[X] <: Cypher[X] with Eq[CN[N]],
    //        D: Order, N: Eq, A](cd: D => CD[D])
    //                           (cn: N => CN[N])
    //                           (implicit $ev1: Order[CD[D]],
    //                                     $ev2: Eq[CN[N]]):
    //     List[(D,N,A)] => D => N => (CD[D], List[CN[N]]) = {
    //   // Combinators
    //   val applyfi: List[(CD[D],CN[N],A)] => CD[D] => CN[N] => (CD[D], List[CN[N]]) =
    //     applyf[CD[D],CN[N],A](implicitly[Order[CD[D]]], implicitly[Eq[CN[N]]])

    //   (ts: List[(D,N,A)]) => (d: D) => (n: N) =>
    //     applyfi(ts.map { case (d,n,a) => (cd(d),cn(n),a) })(cd(d))(cn(n))
    // }

    object Test {
      apply(ts, date, name)
      val applyfi = applyf(implicitly[Order[String]], implicitly[Eq[String]])
      applyfi(ts)(date)(name)

      val ts_HesOrdD_HesEqN_RawA = ts.map {
        case (d,n,a) => (HesEnc withOrd d, HesEnc withEq n, a)
      }
      v1(ts_HesOrdD_HesEqN_RawA, HesEnc.withOrd(date), HesEnc.withEq(name))
      val v1fi = v1f(implicitly[Order[HesOrd[String]]], implicitly[Eq[HesEq[String]]])
      v1fi(ts_HesOrdD_HesEqN_RawA)(HesEnc.withOrd(date))(HesEnc.withEq(name))

      v2(ts_HesOrdD_HesEqN_RawA, HesEnc.withOrd(date), HesEnc.withEq(name))
      val v2fi = v2f(implicitly[Order[HesOrd[String]]], implicitly[Eq[HesEq[String]]])
      v2fi(ts_HesOrdD_HesEqN_RawA)(HesEnc.withOrd(date))(HesEnc.withEq(name))
    }
  }

  object App2 {
    def apply[N: Eq](ts: List[(_,N,_)]) =
      Stats1.mostVisitedClient(ts)
  }

  object App3 {
    def apply[A: Eq](ts: List[(_,_,A)]) =
      Stats2.visitedPlaces(ts)
  }

  // Raw data:
  println(App1(ts, date, name))
  println(App2(ts))
  println(App3(ts))

  // HES data:
  println(App1(ts.map(t => (HesEnc.withOrd(t._1), HesEnc.withEq(t._2), HesEnc.withEq(t._3))),
    HesEnc.withOrd(date), HesEnc.withEq(name)))
  // Databe elements and reference to these elements must use the same
  // encryption scheme
  illTyped("""
  println(App1(ts.map(t => (HesEnc.withOrd(t._1), HesEnc.withEq(t._2), HesEnc.withEq(t._3))),
    date, name))
  """)
  // You could not encrypt date with HesEq. Date requies an Hes
  // encryption scheme with order.
  illTyped("""
  println(App1(ts.map(t => (HesEnc.withEq(t._1), HesEnc.withEq(t._2), HesEnc.withEq(t._3))),
    HesEnc.withEq(date), HesEnc.withEq(name)))
  """)
  println(App2(ts.map(t => (t._1, HesEnc.withEq(t._2), t._3))))
  println(App3(ts.map(t => (t._1, t._2, HesEnc.withEq(t._3)))))

  // AES data:
  // AES has no definition for Eq or Order class type.
  illTyped("""
  println(App1(ts.map(t => (AesEnc(t._1), AesEnc(t._2), AesEnc(t._3))),
    AesEnc(date), AesEnc(name)))
  """)
  // For data with no constraint on it, you could use AES.
  println(App2(ts.map(t => (AesEnc(t._1), t._2, AesEnc(t._3)))))
  println(App3(ts.map(t => (AesEnc(t._1), AesEnc(t._2), t._3))))
}
