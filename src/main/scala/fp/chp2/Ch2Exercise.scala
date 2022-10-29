/*
2.1 Write a recursive function to get the nth Fibonacci number ( http://mng.bz/C29s). 
  The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the previous two—the sequence 
  begins 0, 1, 1, 2, 3, 5. Your definition should use a local tail-recursive function.
*/
def nthFibonacci(n: Int): Int =
  @annotation.tailrec
  def loop(n: Int, prev: Int, cur: Int): Int =
    if n == 0 then prev
    else loop(n - 1, cur, prev + cur)

  loop(n, 1, 1)

/*
2.2 Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function, 
gt, which returns true if the first parameter is greater than the second paramter:
*/
def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = 
  @annotation.tailrec
  def loop(n: Int): Boolean = 
    if n >= as.length - 1 then true
    else if gt(as(n + 1), as(n)) then false
    else loop(n + 1)

  loop(0)


/*
2.3 currying, which converts a function f of two arguments into a function of one argument that partially applies f.
*/
def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

/*
2.4 Implement uncurry, which reverses the transformation of curry. Note that since => associates to the right, 
A => (B => C) can be written as A => B => C.
*/
def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

/*
2.5 Implement the higher-order function that composes two functions.
*/
def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))


@main def ch2Exercise(): Unit = 
  val r1 = isSorted(Array(6, 5, 4, 3, 2, 1), (x, y) => x > y)
  println(s"r1 = $r1")

  val r2 = isSorted(Array(6, 5, 14, 31, 62, 1), (x, y) => x > y)
  println(s"r2 = $r2")

  val r3 = isSorted(Array("blue", "car", "deposit", "from", "there"), (a, b) => a < b)
  println(s"r3 = $r3")


