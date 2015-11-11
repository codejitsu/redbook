package redbook

import scala.annotation.tailrec

object Chapter03 {
  sealed trait List[+A] // red book list
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def init[A](l: List[A]): List[A] = l match {
      case Nil | Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    @tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
      case l @ Cons(_, _) => l
    }

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case _ if n == 0 => l
      case Cons(_, xs) => drop(xs, n - 1)
    }

    def setHead[A](list: List[A], h: A): List[A] = list match {
      case Nil => List(h)
      case Cons(_, t) => Cons(h, t)
    }

    def tail[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(_, t) => t
    }

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }
}
