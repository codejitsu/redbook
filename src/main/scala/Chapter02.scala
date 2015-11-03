package redbook

import scala.annotation.tailrec

object Chapter02 {
  def fib(n: Int): Int = {

    @tailrec
    def go(i: Int, f1: Int, f2: Int): Int = {
      if (i <= 0) 0
      else if (i == 1) f1
      else if (i == 2) f2
      else go(i - 1, f2, f1 + f2)
    }

    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean = {
      if (as.isEmpty) true
      else if (i == as.length - 1) true
      else if (ordered(as(i), as(i + 1))) {
        loop(i + 1)
      } else false
    }

    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
