import org.scalatest.{FlatSpec, Matchers}

import redbook.Chapter03._
import redbook.Chapter03.List._

class Chapter03Test extends FlatSpec with Matchers {
  "setHead of an empty list" should "return one element list" in {
    val list = Nil

    setHead(list, 1) should be (List(1))
  }

  "setHead of a non empty list" should "replace the head and return the new list" in {
    val list = List(1, 2, 3, 4)

    setHead(list, 5) should be (List(5, 2, 3, 4))
  }

  "tail of an empty list" should "return Nil" in {
    val list = Nil

    tail(list) should be (Nil)
  }

  "tail of a non empty list" should "remove the head and return the tail of the list" in {
    val list = List(1, 2, 3, 4)

    tail(list) should be (List(2, 3, 4))
  }

  "match expression" should "be evaluated to 3" in {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    x should be (3)
  }
}
