import org.scalatest.{FlatSpec, Matchers}

import redbook.Chapter03._
import redbook.Chapter03.List._

class Chapter03Test extends FlatSpec with Matchers {
  "map" should "return mapped list for non empty list" in {
    map[Int, String](List(1, 2, 3, 4, 5))(_.toString) should be (List("1", "2", "3", "4", "5"))
  }

  "map" should "return Nil for Nil" in {
    map[Int, String](Nil)(_.toString) should be (Nil)
  }

  "dToStr" should "return a list containing string representations for nonempty list" in {
    dToStr(List(1.0, 2.0,3.0)) should be (List("1.0", "2.0", "3.0"))
  }

  "dToStr" should "return Nil for empty list" in {
    dToStr(Nil) should be (Nil)
  }

  "plusone" should "return a list containing incremented integers for nonempty list" in {
    plusone(List(1,2,3)) should be (List(2,3,4))
  }

  "plusone" should "return Nil for empty list" in {
    plusone(Nil) should be (Nil)
  }

  "concat" should "concat two lists together" in {
    concat(List(1, 2, 3), List(4, 5, 6)) should be (List(1, 2, 3, 4, 5, 6))
  }

  "concat of Nil with something" should "be something" in {
    concat(Nil, List(1, 2, 3)) should be (List(1, 2, 3))
  }

  "concat of something with Nil" should "be something" in {
    concat(List(1, 2, 3), Nil) should be (List(1, 2, 3))
  }

  "concat of two Nils" should "be a Nil" in {
    concat(Nil, Nil) should be (Nil)
  }

  "appendFL" should "add a new element to the begin of a list" in {
    appendFL(List(2, 3, 4), 1) should be (List(1, 2, 3, 4))
  }

  "appendFL" should "add a new element to the Nil" in {
    appendFL(Nil, 1) should be (List(1))
  }

  "reverse" should "return reverse order list for an nonempty list" in {
    reverse(List(1, 2, 3, 4, 5)) should be (List(5, 4, 3, 2, 1))
  }

  "reverse" should "return Nil for an empty list" in {
    reverse(Nil) should be (Nil)
  }

  "lengthFL" should "return an integer for an nonempty list" in {
    listLengthFL(List(1, 2, 3, 4, 5)) should be (5)
  }

  "lengthFL" should "return 0 for an empty list" in {
    listLengthFL(Nil) should be (0)
  }

  "productFL" should "return an integer for an nonempty list" in {
    productFL(List(1, 2, 3, 4, 5)) should be (120)
  }

  "productFL" should "return 0 for an empty list" in {
    productFL(Nil) should be (1)
  }

  "sumFL" should "return an integer for an nonempty list" in {
    sumFL(List(1, 2, 3, 4, 5)) should be (15)
  }

  "sumFL" should "return 0 for an empty list" in {
    sumFL(Nil) should be (0)
  }

  "foldLeft" should "be tail recursive: nonempty list" in {
    val len = foldLeft(List(1, 2, 3, 4, 5), 0)((i, _) => i + 1)

    len should be (5)
  }

  "foldLeft" should "be tail recursive: empty list" in {
    val len = foldLeft[Int, Int](Nil, 0)((i, _) => i + 1)

    len should be (0)
  }

  "length" should "return an integer for an nonempty list" in {
    listLength(List(1, 2, 3, 4, 5)) should be (5)
  }

  "length" should "return 0 for an empty list" in {
    listLength(Nil) should be (0)
  }

  "init" should "return list without last element" in {
    init(List(1, 2, 3, 4, 5)) should be (List(1, 2, 3, 4))
  }

  "init" should "return Nil on one element list" in {
    init(List(1)) should be (Nil)
  }

  "init" should "return Nil on empty list" in {
    init(Nil) should be (Nil)
  }

  "drop" should "retrun the same list if f => false" in {
    val list = List(1, 2, 3, 4, 5, 6, 7)

    dropWhile(list, (x : Int) => false) should be(list)
  }

  "dropWhile" should "remove the first elements from list" in {
    val list = List(1, 2, 3, 4, 5, 6, 7)

    dropWhile(list, (x : Int) => x < 4) should be(List(4, 5, 6, 7))
  }

  "dropWhile" should "return empty list if _ => x <= 7" in {
    val list = List(1, 2, 3, 4, 5, 6, 7)

    dropWhile(list, (x : Int) => x <= 7) should be(Nil)
  }

  "dropWhile" should "return Nil on empty list" in {
    val list = Nil

    dropWhile(list, (x : Int) => true) should be(Nil)
  }

  "drop" should "retrun the same list if n == 0" in {
    val list = List(1, 2, 3, 4, 5, 6, 7)

    drop(list, 0) should be(list)
  }

  "drop" should "remove the first n elements from list" in {
    val list = List(1, 2, 3, 4, 5, 6, 7)

    drop(list, 3) should be(List(4, 5, 6, 7))
  }

  "drop" should "return empty list if n == list.size" in {
    val list = List(1, 2, 3, 4, 5, 6, 7)

    drop(list, 7) should be(Nil)
  }

  "drop" should "return Nil on empty list" in {
    val list = Nil

    drop(list, 10) should be(Nil)
  }

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
