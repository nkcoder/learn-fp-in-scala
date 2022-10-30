enum MyTree[+A]:
  case Leaf(value: A)
  case Branch(left: MyTree[A], right: MyTree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size


object MyTree:
  extension (t: MyTree[Int]) def firstPositive: Option[Int] = t match
    case Leaf(i) => if i > 0 then Some(i) else None
    case Branch(l, r) => l.firstPositive orElse r.firstPositive


@main def tree(): Unit = 
  import MyTree.{Leaf, Branch}
  val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  val size = tree.size
  println(s"size = $size")

  val firstPositive = tree.firstPositive
  println(s"firstPositive = $firstPositive")


