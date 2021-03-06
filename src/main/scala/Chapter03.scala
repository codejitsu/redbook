package redbook

import scala.annotation.tailrec

object Chapter03 {
  sealed trait List[+A] // red book list
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def mapF[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](tree, z => Leaf(f(z)))(Branch(_, _))

    def depthF(tree: Tree[Int]): Int = fold[Int, Int](tree, z => 0)((l, r) => 1 + (l max r))

    def maximumF(tree: Tree[Int]): Int = fold[Int, Int](tree, z => z)(_ max _)

    def sizeF[A](tree: Tree[A]): Int = fold[A, Int](tree, z => 1)(1 + _ + _)

    def fold[A,B](tree: Tree[A], z: A => B)(f: (B, B) => B): B = tree match {
      case Leaf(a) => z(a)
      case Branch(l, r) => f(fold(l, z)(f), fold(r, z)(f))
    }

    def mapT[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(mapT(l)(f), mapT(r)(f))
    }

    def depth(tree: Tree[Int]): Int = tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    def maximum(tree: Tree[Int]): Int = tree match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

    def sizeT[A](tree: Tree[A]): Int = {
      @tailrec
      def size(trees: List[Tree[A]], acc: Int): Int = trees match {
        case Nil => acc
        case Cons(tree, ts) =>
          tree match {
            case Leaf(_) => size(ts, acc + 1)
            case Branch(left, right) => size(List.appendFL(List.appendFL(ts, left), right), acc + 1)
          }
      }

      size(List(tree), 0)
    }
  }

  object List {
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (Nil, Nil) => true
      case (Cons(_, _), Nil) => true
      case (Nil, Cons(_, _)) => false
      case (Cons(h1, t1), Cons(h2, t2)) => {
        if (h1 != h2) hasSubsequence(t1, sub)
        else {
          def startsWith[B](f: List[B], s: List[B]): Boolean = (f, s) match {
            case (_, Nil) => true
            case (Nil, _) => false
            case (Cons(f1, _), Cons(s1, _)) if (f1 != s1) => false
            case (Cons(f1, ft), Cons(s1, st)) => startsWith(ft, st)
          }

          startsWith(t1, t2)
        }
      }
    }

    def addZW(first: List[Int], second: List[Int]): List[Int] = zipWith(first)(second)((a, b) => a + b)

    def zipWith[A](first: List[A])(second: List[A])(f: (A, A) => A): List[A] = (first, second) match {
      case (Nil, Nil) => Nil
      case (Nil, _) => second
      case (_, Nil) => first
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1)(t2)(f))
    }

    def add(first: List[Int], second: List[Int]): List[Int] = (first, second) match {
      case (Nil, Nil) => Nil
      case (Nil, _) => second
      case (_, Nil) => first
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, add(t1, t2))
    }

    def filterFM[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
      foldLeft[List[B], List[B]](map(as)(f), Nil)((b, a) => concat(b, a))

    def filter[A](as: List[A])(f: A => Boolean): List[A] = reverse(foldLeft[A, List[A]](as, Nil) {
      case (b, a) if f(a) => Cons(a, b)
      case (b, _) => b
    })

    def map[A,B](as: List[A])(f: A => B): List[B] = reverse(foldLeft[A, List[B]](as, Nil)((b, a) => Cons(f(a), b)))

    def dToStr(l: List[Double]): List[String] = reverse(foldLeft[Double, List[String]](l, Nil)((b, a) => Cons(a.toString, b)))

    def plusone(l: List[Int]): List[Int] = reverse(foldLeft[Int, List[Int]](l, Nil)((b, a) => Cons(a + 1, b)))

    def concat[A](first: List[A], second: List[A]): List[A] = foldRight[A, List[A]](first, second)((b, a) => Cons(b, a))

    def appendFL[A](l: List[A], elem: A): List[A] = reverse(foldLeft[A, List[A]](l, List(elem))((r, a) => Cons(a, r)))

    def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil)((r, a) => Cons[A](a, r))

    def listLengthFL[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

    def productFL(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

    def sumFL(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

    @tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def listLength[A](l: List[A]): Int = foldRight(l, 0)((_, i) => i + 1)

    def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

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
