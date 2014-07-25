sealed abstract class Tree[T]
case class Item[T] (val item: T) extends Tree[T]
case class Section[T] (val trees: List[Tree[T]]) extends Tree[T]

sealed abstract class Path[T]
case class Top[T]() extends Path[T]
case class Node[T] (val ysibling: List[Tree[T]],
                    val path: Path[T],
                    val esibling: List[Tree[T]]) extends Path[T]

sealed abstract class Location[T]
class Loc[T] (val current: Tree[T],
              val path: Path[T]) extends Location[T] {

  def this(t: Tree[T]) = this(t, Top())

  def goLeft: Loc[T] = path match {
    case Top() => sys.error("left of top")
    case Node(y :: ysibling, up, esibling) =>
      Loc(y, Node(ysibling, up, current :: esibling))
    case Node(Nil, up, esibling) => sys.error("left of first")
  }

  def goRight: Loc[T] = path match {
    case Top() => sys.error("right of top")
    case Node(ysibling, up, e :: esibling) =>
      Loc(e, Node(current :: ysibling, up, esibling))
    case Node(ysibling, up, Nil) => sys.error("right of last")
  }

  def goUp: Loc[T] = path match {
    case Top() => sys.error("up of top")
    case Node(ysibling, up, esibling) =>
      Loc(Section(ysibling.reverse ++ (current :: esibling)), up)
  }

  def goDown: Loc[T] = current match {
    case Item(_) => sys.error("down of item")
    case Section(t :: ts) => Loc(t, Node(Nil, path, ts))
    case _ => sys.error("down of empty")
  }

  def goNth(nth: Int): Loc[T] = nth match {
    case 1 => goDown
    case _ => if (nth > 0) { goRight.goNth(nth - 1) }
              else { sys.error("goNth expects a positive Int") }
  }

  def change(t: Tree[T]): Loc[T] = Loc(t, path)

  def insertRight(r: Tree[T]): Loc[T] = path match {
    case Top() => sys.error("insert of top")
    case Node(ysibling, up, esibling) =>
      Loc(current, Node(ysibling, up, r :: esibling))
  }

  def insertLeft(l: Tree[T]): Loc[T] = path match {
    case Top() => sys.error("insert of top")
    case Node(ysibling, up, esibling) =>
      Loc(current, Node(l :: ysibling, up, esibling))
  }

  def insertDown(t: Tree[T]): Loc[T] = current match {
    case Item(_) => sys.error("down of item")
    case Section(sons) => Loc(t, Node(Nil, path, sons))
  }

  def delete: Loc[T] = path match {
    case Top() => sys.error("delete of top")
    case Node(ysibling, up, e :: esibling) =>
      Loc(e, Node(ysibling, up, esibling))
    case Node(y :: ysibling, up, esibling) =>
      Loc(y, Node(ysibling, up, esibling))
    case Node(Nil, up, Nil) => Loc(Section(Nil), up)
  }
}

object Loc {
  def apply[T](current: Tree[T], path: Path[T]) =
    new Loc[T](current, path)
  def apply[T](current: Tree[T]) =
    new Loc[T](current)

  def main(args: Array[String]) {
    // a * b + c * d
    val tree = Section(List(
      Section(Item("a") :: Item("*") :: Item("b") :: Nil),
      Item("+"),
      Section(Item("c") :: Item("*") :: Item("d") :: Nil)))

    // a * b + c * d
    //     loc --^
    val expectedStarLoc  = Loc(
      Item("*"),
      Node(Item("c") :: Nil,
           Node(Item("+") ::
                Section(Item("a") ::
                        Item("*") ::
                        Item("b") :: Nil) :: Nil,
                Top[String],
                Nil),
           Item("d") :: Nil))


    val starLoc = Loc(tree).goDown.goRight.goRight.goDown.goRight

    assert (starLoc.path.toString == expectedStarLoc.path.toString,
      { println("Bad path construction") })
  }
}
