object ListsDemo:
  @main def run(): Unit = 
    val xs = List(23, 14, 98, 100, 51, 99, 17)
    val take2 = xs.take(2) 
    val takeRight3 = xs.takeRight(3)
    val takeWhile = xs.takeWhile(_ > 100)
    val forAll = xs.forall(_ % 2 == 0)
    val exists = xs.exists(_ == 98)
    val scanLeft = xs.scanLeft(0)(_ + _)
    val scanRight = xs.scanRight(1)(_ + _)
    val foldLeft = xs.foldLeft(0)(_ + _)
    val foldRight = xs.foldRight(1)(_ * _)

    println(s"take2 = $take2")
    println(s"takeRight3= $takeRight3")
    println(s"takeWhile= $takeWhile")
    println(s"forAll= $forAll")
    println(s"exists= $exists")
    println(s"scanLeft= $scanLeft")
    println(s"scanRight= $scanRight")
    println(s"foldLeft= $foldLeft")
    println(s"foldRigth= $foldRight")

