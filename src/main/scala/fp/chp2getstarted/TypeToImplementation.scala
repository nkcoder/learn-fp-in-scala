package fp.chp2getstarted

def partial[A, B, C](a: A, f: (A, B) => C): B => C = 
  b => f(a, b)

@main def typeToImplementation(): Unit = 
  val f = (x: Double) => math.Pi / 2 - x
  val g = math.sin
  val cos = f andThen g
  val cos2 = g compose f
  println(s"cos = $cos, cos2 = $cos2")
  println(s"cos = ${cos(10)}, cos2 = ${cos2(10)}")

