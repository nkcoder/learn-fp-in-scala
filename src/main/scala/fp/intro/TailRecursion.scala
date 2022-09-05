package fp.intro

// factorial: f(n) = 1 * 2 * ... * (n - 1) * n
def factorial(n: Int): Int =
  @annotation.tailrec
  def loop(n: Int, acc: Int): Int =
    if n <= 0 then acc
    else loop(n - 1, n * acc)

  loop(n, 1)

// fibonacci: f(0) = 1, f(1) = 1, f(n) = f(n-1) + f(n-2)
def fibonacci(n: Int): Int =
  def loop(n: Int, prev: Int, cur: Int): Int =
    if n == 0 then prev
    else loop(n - 1, cur, prev + cur)

  loop(n, 1, 1)




@main def tailRec(): Unit =
  val fac = factorial(5)
  println(s"factorial(5) = $fac")

  val fib = fibonacci(8)
  println(s"fibonacci(5) = $fib")


