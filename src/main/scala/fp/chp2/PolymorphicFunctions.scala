def findFirst[A](as: Array[A], p: A => Boolean): Int =
  @annotation.tailrec
  def go(n: Int): Int = 
    if n >= as.length then -1
    else if p(as(n)) then n
    else go(n + 1)

  go(0)


@main def polymorphicFunctions() = 
  val r1 = findFirst(Array("hello", "WORLD", "go"), s => s.startsWith("W"))
  val r2 = findFirst(Array(1, 3, 5, 6, 9, 8, 2, 1), i => i > 5) 
  println(s"r1 = $r1, r2 = $r2")

