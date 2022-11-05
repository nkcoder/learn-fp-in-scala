// hide standard library Option as we're writing our own version
import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Some(x) => Some(f(x))
    case None => None

  def flatMap[B](f: A => Option[B]): Option[B] = this match 
    case Some(x) => f(x)
    case None => None

  def getOrElse[B >: A](default: => B): B = this match 
    case Some(x) => x
    case None => default

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match 
    case Some(x) => this
    case None => ob

  def filter(f: A => Boolean): Option[A] = this match 
    case Some(x) if f(x) => this
    case _ => None


object Option:
  def mean(xs: Seq[Double]): Option[Double] = 
    if xs.isEmpty then None 
    else Some(xs.sum / xs.length)

  def lift[A, B](f: A => B): Option[A] => Option[B] = (x: Option[A]) => x.map(f)

  def lift0[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  def abs0: Option[Double] => Option[Double] = lift0(math.abs)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for 
      aa <- a
      bb <- b
    yield f(aa, bb)

  def sequence[A](as: List[Option[A]]): Option[List[A]] = 
    as match
      case Nil => Some(Nil)
      case head :: next => head.flatMap(hh => sequence(next).map(hh :: _))

  def sequence2[A](as: List[Option[A]]): Option[List[A]] = 
    as.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = 
    as.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  @main def main(): Unit = 
    val absValue = abs0(Some(100.5))
    println(s"absValue = $absValue")

    val invalidAbsValue = abs0(None)
    println(s"invalidAbsValue = $invalidAbsValue")

    val max = map2(Some(10), Some(20))(math.max)
    println(s"max = $max")

