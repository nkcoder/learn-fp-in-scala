package fp.chp2

import scala.annotation.tailrec

/**
 * For data types, it’s a common idiom to have a variadic apply method in the companion object
 * to conveniently construct instances of the data type.
 */
enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def length[A](as: List[A]): Int = as match
    case Nil => 0
    case Cons(_, t) => 1 + length(t)

  def sum(ints: List[Int]): Int = ints match
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(head, tail) => head * product(tail)

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) = foldRight(ns, 0, _ + _)

  def productViaFoldRight(ns: List[Double]) = foldRight(ns, 1.0, _ * _)

  // The special * annotation allows us to pass a Seq to a variadic method.
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail *))

  def append[A](a1: List[A], b1: List[A]): List[A] = a1 match
    case Nil => b1
    case Cons(h, t) => Cons(h, append(t, b1))


  /**
   * exercise 3.2
   * Implement the function tail for removing the first element of a List. Note that the function takes constant time.
   * What are different choices you could make in your implementation if the List is Nil? We’ll return to this question
   * in the next chapter.
   */
  def tail[A](as: List[A]) = as match
    case Nil => Nil
    case Cons(_, t) => t

  /**
   * exercise 3.3
   * Using the same idea, implement the function setHead for replacing the first element of a List with a different
   * value.
   */
  def setHead[A](as: List[A], head: A) = as match
    case Nil => Nil
    case Cons(_, t) => Cons(head, t)

  /**
   * exercise 3.4
   * Generalize tail to the function drop, which removes the first n elements from a list. Note that this function
   * takes time proportional only to the number of elements being dropped—we don’t need to make a copy of the entire
   * List.
   */
  def drop[A](as: List[A], n: Int): List[A] =
    @annotation.tailrec
    def loop(i: Int, r: List[A]): List[A] =
      if i == n then r
      else loop(i + 1, tail(r))

    loop(0, as)

  /**
   * exercise 3.5 TODO
   * Implement dropWhile, which removes elements from the List prefix as long
   * as they match a predicate.
   *
   */
  @annotation.tailrec
  def dropWhile[A](as: List[A], p: A => Boolean): List[A] = as match
    case Nil => Nil
    case Cons(h, _) if !p(h) => as
    case Cons(h, t) if p(h) => dropWhile(t, p)

  /**
   * exercise 3.6
   * Not everything works out so nicely. Implement a function, init, that returns a List consisting of all but the last
   * element of a List. So, given List(1,2,3,4) , init will return List(1,2,3). Why can’t this function be implemented
   * in constant time like tail?
   */
  def init[A](as: List[A]): List[A] = as match
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))

  /**
   * exercise 3.7 TODO
   * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
   * Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list. This is a
   * deeper question that we’ll return to in chapter 5.
   */

  /**
   * exercise 3.8 TODO
   * See what happens when you pass Nil and Cons themselves to foldRight,
   * like this: foldRight(List(1, 2, 3), Nil: List[Int], Cons(_, _)).24 What do you think this says about the
   * relationship between foldRight and the data constructors of List?
   */
  // val fold1 = foldRight(List(1, 2, 3), Nil: List[Int], Cons(_, _))
  // println(s"fold1 = $fold1") // fold1 = Cons(1,Cons(2,Cons(3,Nil)))

  /**
   * exercise 3.9
   * Compute the length of a list using foldRight.
   */
  def lengthViaFoldRight[A](as: List[A]): Int = foldRight(as, 0, (_, y) => 1 + y)

  /**
   * exercise 3.10
   * Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError for large lists
   * (we say it’s not stack-safe). Convince yourself that this is the case, and then write another general list-recursion
   * function, foldLeft, that is tail-recursive, using the techniques we discussed in
   * the previous chapter. Here is its signature
   */
  // def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = ()



@main def definition(): Unit =
  import List.*
  /**
   * What will be the result of the following match expression?
   */
  val r1 = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  println(s"r1 = $r1")

  val nums1 = List(1, 2, 5, 4, 90, 10, 45)
  println(s"tail = ${tail(nums1)}, new list with new head = ${setHead(nums1, 100)}")

  val nums2 = List(1, 2, 3, 4, 5)
  println(s"drop 2 = ${drop(nums2, 2)}")

  val nums3 = List(124, 155, 109, 90, 54)
  println(s"drop while > 100 = ${dropWhile(nums3, x => x > 100)}")

  val nums4 = List(10, 20, 30, 40, 50)
  println(s"init = ${init(nums4)}")

  val fold1 = foldRight(List(1, 2, 3), Nil: List[Int], Cons(_, _))
  println(s"fold1 = $fold1") // fold1 = Cons(1,Cons(2,Cons(3,Nil)))

  val length1 = lengthViaFoldRight(List(1, 3, 5, 7, 9, 11, 0))
  println(s"length1 = $length1")


