def partial[A, B, C](a: A, f: (A, B) => C): B => C = 
  (b: B) => f(a, b)

def curry2[A, B, C](f: (A, B) => C): A => (B => C) = 
  (a: A) => ((b: B) => f(a, b))

def curry3[A, B, C](f: (A, B) => C): A => (B => C) =
  a => b => f(a, b)

def uncurry2[A, B, C](f: A => B => C): (A, B) => C = 
  (a: A, b: B) => f(a)(b)

def uncurry3[A, B, C](f: A => B => C): (A, B) => C = 
  (a, b) => f(a)(b)

def compose2[A, B, C](f: B => C, g: A => B): A => C = 
  a => f(g(a))

@main def typeToImplementation(): Unit = 
  val f = (x: Double) => math.Pi / 2 - x
  val g = math.sin
  val cos = f andThen g
  val cos2 = g compose f
  println(s"cos = $cos, cos2 = $cos2")
  println(s"cos = ${cos(10)}, cos2 = ${cos2(10)}")

