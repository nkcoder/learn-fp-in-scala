package fp.chp7

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future as _
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Callable

opaque type Future[+A] = (A => Unit) => Unit
opaque type Par[+A] = ExecutorService => Future[A]

extension [A](pa: Par[A])
  def run(es: ExecutorService): A =
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    pa(es) { a =>
      ref.set(a); latch.countDown
    }
    latch.await()
    ref.get

def unit[A](a: A): Par[A] = es => callback => callback(a)

def fork[A](a: => Par[A]): Par[A] =
  es => cb => eval(es)(a(es)(cb))

def eval(es: ExecutorService)(r: => Unit): Unit =
  es.submit(new Callable[Unit] { def call = r })
