package fp.chp10

/** A monoid consists of the following:
  *   - Some type A
  *   - An associative binary operation, combine, that takes two values of type A and combines them
  *     into one: combine(combine(x, y), z) == combine(x, combine(y, z)) for any choice of x: A, y:
  *     A, z: A
  *   - A value, empty: A, that is an identity for that operation: combine(x, empty) == x and
  *     combine(empty, x) == x
  *
  * So Monoid is a type, together with the monoid operations and a set of laws.
  *
  * Just what is a monoid, then? It's simply a type A and an implementation of Monoid[A] that
  * satisfies the laws. Stated tersely, a monoid is a type together with a binary operation
  * (combine) over that type, satisfying associativity and having an identity element (empty).
  */
trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  /** Scala can infer that we're instantiating a new anonymous subtype of Monoid[String] here,
    * allowing us to write new: instead of new Monoid[String]:
    */
  val stringMonoid: Monoid[String] = new:
    override def combine(a1: String, a2: String): String = a1 + a2
    override def empty: String = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    override def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def empty: List[A] = Nil

  val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 + a2
    def empty: Int = 0

  val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 * a2
    def empty: Int = 1

  def booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def empty: Boolean = false

  def booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def empty: Boolean = true

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    def empty: Option[A] = None

object MonoidApp extends App:
  import Monoid.{stringMonoid, listMonoid}

  private val strCombine = stringMonoid.combine("hello", " world")
  println(s"strCombine: $strCombine")

  private val listCombine = listMonoid[Int].combine(List(1, 2, 3), List(4, 5))
  println(s"listCombine: $listCombine")
