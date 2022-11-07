package fp.chp5

import scala.collection.mutable.ListBuffer
import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, t) => Some(h())

  def toListRecursive: List[A] = this match 
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive

  def toListTailRec: List[A] = 
    @annotation.tailrec
    def go(lazyList: LazyList[A], acc: List[A]): List[A] =
      lazyList match 
        case Empty => acc.reverse
        case Cons(h, t) => go(t(), h() :: acc)

    go(this, Nil)

  def toListFast: List[A] = 
    val buf = new ListBuffer[A]
    def go(lazyList: LazyList[A]): List[A] = 
      lazyList match 
        case Empty => buf.toList
        case Cons(h, t) => 
          buf += h() 
          go(t())

    go(this)


  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty

  @annotation.tailrec
  final def drop(n: Int): LazyList[A] = this match 
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this

  def takeWhile(f: A => Boolean): LazyList[A] = this match 
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => empty
    

object LazyList:
  def cons[A](h: => A, t: => LazyList[A]): LazyList[A] =
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] = 
    if as.isEmpty then Empty
    else cons(as.head, apply(as.tail*))

  


