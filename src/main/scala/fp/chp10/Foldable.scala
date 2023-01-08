package fp.chp10

import Monoid.stringMonoid

/**
 * Monoids have an intimate connection with lists.
 */
class Foldable:
  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    combineAll(as.map(f), m)

object FoldableApp extends App:
  /** Note that it doesn't matter if we choose foldLeft or foldRight when folding with monoid: we
    * should get the same result.
    *
    * This is precisely because the laws of associativity and identity hold. A left fold associates
    * operations to the left, whereas a right fold associates to the right, with the identity
    * element on the left and right respectively.
    */
  private val words = List("Hic", "Est", "Index")
  // words.foldRight("")(_ + _) == "Hic" + ("Est" + ("Index" + ""))
  private val foldRight = words.foldRight(stringMonoid.empty)(stringMonoid.combine)
  // words.foldLeft("")(_ + _) == (("" + "Hic") + "Est") + "Index"
  private val foldLeft = words.foldLeft(stringMonoid.empty)(stringMonoid.combine)
  println(s"foldRight: $foldRight, foldLeft: $foldLeft")
