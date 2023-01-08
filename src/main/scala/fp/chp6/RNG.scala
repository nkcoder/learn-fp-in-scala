package fp.chp6

trait RNG:
  def nextInt: (Int, RNG)

type Rand[+A] = RNG => (A, RNG)

object RNG:
  case class SimpleRandom(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRandom = SimpleRandom(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRandom)

  def nonNegativeInt(random: RNG): (Int, RNG) =
    val (n, r) = random.nextInt
    val n0 = if n < 0 then math.abs(n + 1) else n
    (n0, r)

  def double(random: RNG): (Double, RNG) =
    val (n, r) = nonNegativeInt(random)
    (n.toDouble / Int.MaxValue, r)

  def ints(count: Int)(random: RNG): (List[Int], RNG) =
    if count <= 0 then (List(), random)
    else
      val (n, r) = random.nextInt
      val (n0, r0) = ints(count - 1)(r)
      (n :: n0, r0)

  def ints2(count: Int)(random: RNG): (List[Int], RNG) =
    def go(c: Int, r: RNG, result: List[Int]): (List[Int], RNG) =
      if c <= 0 then (List(), r)
      else
        val (n1, r1) = r.nextInt
        go(c - 1, r1, n1 :: result)

    go(count, random, List())

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  @main def main(): Unit =
    val random = SimpleRandom(100)

    val (n1, r1) = random.nextInt
    println(s"n1 = $n1, r1 = $r1")
    val (n2, r2) = r1.nextInt
    println(s"n2 = $n2, r2 = $r2")

    val (nn, nr) = nonNegativeInt(random)
    println(s"nn = $nn, nr = $nr")
    val (nn2, nr2) = nonNegativeInt(nr)
    println(s"nn2 = $nn2, nr2: $nr2")

    val (dv, dr) = double(random)
    println(s"dv = $dv, dr = $dr")

    val (lists, lr) = ints(10)(random)
    println(s"lists = $lists, lr = $lr")

    val (lists2, lr2) = ints2(10)(random)
    println(s"lists2 = $lists2, lr2 = $lr2")

  opaque type State[S, +A] = S => (A, S)

  object State:
    extension [S, A](underlying: State[S, A])
      def run(s: S): (A, S) = underlying(s)

      def flatMap[B](f: A => State[S, B]): State[S, B] =
        s =>
          val (a, s1) = underlying(s)
          f(a)(s1)

      def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

    def apply[S, A](f: S => (A, S)): State[S, A] = f

    def unit[S, A](a: A): State[S, A] = s => (a, s)
