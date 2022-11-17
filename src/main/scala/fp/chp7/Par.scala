package fp.chp7

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  extension [A](pa: Par[A]) def run(s: ExecutorService): Future[A] = pa(s)

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  private case class UnitFuture[A](value: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = value
    def isCancelled(): Boolean = false
    def cancel(mayInterruptIfRunning: Boolean): Boolean = false

  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) =>
      val futureA = pa(es)
      val futureB = pb(es)
      UnitFuture(f(futureA.get, futureB.get))

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] { def call = a(es).get })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  extension [A](pa: Par[A])
    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = parList.map(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((pa, acc) => pa.map2(acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork {
      val pars: List[Par[List[A]]] = as.map(asyncF(a => if f(a) then List(a) else List()))
      sequence(pars).map(_.flatten)
    }
