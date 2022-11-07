package fp.chp5

object Strictness:

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if cond then onTrue else onFalse

  def maybeTwice(b: Boolean, i: => Int) = if b then i + i else 0

  def maybeTwiceLazy(b: Boolean, i: => Int) = 
    lazy val j = i
    if b then j + j else 0

  @main def main(): Unit = 
    def square(x: Double) = x * x
      println(s"square(10.5) = ${square(10.5)}")

    10 == 10.1 && {println("it's equal"); true}

    if2(true, 10, sys.error("fail"))

    val x = maybeTwice(true, { println("hi"); 100 })
    println(s"x = $x")

    val y = maybeTwiceLazy(true, { println("hi"); 100 })
    println(s"y = $y")
