import org.scalatest.{FlatSpec, Matchers}

import redbook.Chapter03._
import redbook.Chapter03.List._
import redbook.Chapter03.Tree._

class Chapter03Test extends FlatSpec with Matchers {
  "mapF" should "handle branch nodes" in {
    mapF(Branch(Branch(Leaf(1), Leaf(7)), Branch(Branch(Leaf(0), Leaf(1)), Leaf(4))))(_.toString) should be
    (Branch(Branch(Leaf("1"), Leaf("7")), Branch(Branch(Leaf("0"), Leaf("1")), Leaf("4"))))
  }

  "mapF" should "handle leaf nodes" in {
    mapF(Leaf(1))(_.toString) should be (Leaf("1"))
  }

  "depthF" should "return maximal path" in {
    depthF(Branch(Branch(Leaf(1), Leaf(7)), Branch(Branch(Leaf(0), Leaf(1)), Leaf(4)))) should be (3)
  }

  "depthF" should "return 2 for 3-level tree" in {
    depthF(Branch(Branch(Leaf(1), Leaf(7)), Branch(Leaf(1), Leaf(4)))) should be (2)
  }

  "depthF" should "return 1 for 3 node complete tree" in {
    depthF(Branch(Leaf(1), Leaf(2))) should be (1)
  }

  "depthF" should "return 0 for a leaf" in {
    depthF(Leaf(1)) should be (0)
  }

  "maximumF" should "return 7 for a tree with 4 leaves and values 1, 7, 1, 4" in {
    maximumF(Branch(Branch(Leaf(1), Leaf(7)), Branch(Leaf(1), Leaf(4)))) should be (7)
  }

  "maximumF" should "return 2 for a node with 2 leaves and values 1, 2" in {
    maximumF(Branch(Leaf(1), Leaf(2))) should be (2)
  }

  "maximumF" should "return 1 for a leaf node with value 1" in {
    maximumF(Leaf(1)) should be (1)
  }

  "sizeF" should "return 7 for a tree with 4 leaves" in {
    sizeF(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) should be (7)
  }

  "sizeF" should "return 3 for a node with 2 leaves" in {
    sizeF(Branch(Leaf("a"), Leaf("b"))) should be (3)
  }

  "sizeF" should "return 1 for a leaf node" in {
    sizeF(Leaf("a")) should be (1)
  }

  "map" should "handle branch nodes" in {
    mapT(Branch(Branch(Leaf(1), Leaf(7)), Branch(Branch(Leaf(0), Leaf(1)), Leaf(4))))(_.toString) should be
      (Branch(Branch(Leaf("1"), Leaf("7")), Branch(Branch(Leaf("0"), Leaf("1")), Leaf("4"))))
  }

  "map" should "handle leaf nodes" in {
    mapT(Leaf(1))(_.toString) should be (Leaf("1"))
  }

  "depth" should "return maximal path" in {
    depth(Branch(Branch(Leaf(1), Leaf(7)), Branch(Branch(Leaf(0), Leaf(1)), Leaf(4)))) should be (3)
  }

  "depth" should "return 2 for 3-level tree" in {
    depth(Branch(Branch(Leaf(1), Leaf(7)), Branch(Leaf(1), Leaf(4)))) should be (2)
  }

  "depth" should "return 1 for 3 node complete tree" in {
    depth(Branch(Leaf(1), Leaf(2))) should be (1)
  }

  "depth" should "return 0 for a leaf" in {
    depth(Leaf(1)) should be (0)
  }

  "maximum" should "return 7 for a tree with 4 leaves and values 1, 7, 1, 4" in {
    maximum(Branch(Branch(Leaf(1), Leaf(7)), Branch(Leaf(1), Leaf(4)))) should be (7)
  }

  "maximum" should "return 2 for a node with 2 leaves and values 1, 2" in {
    maximum(Branch(Leaf(1), Leaf(2))) should be (2)
  }

  "maximum" should "return 1 for a leaf node with value 1" in {
    maximum(Leaf(1)) should be (1)
  }

  "size" should "return 7 for a tree with 4 leaves" in {
    sizeT(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) should be (7)
  }

  "size" should "return 3 for a node with 2 leaves" in {
    sizeT(Branch(Leaf("a"), Leaf("b"))) should be (3)
  }

  "size" should "return 1 for a leaf node" in {
    sizeT(Leaf("a")) should be (1)
  }

  "hasSubsequence" should "return true for second argument being subsequence of the first one" in {
    hasSubsequence(List(1, 2, 3, 4), List(1, 2)) should be (true)
    hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should be (true)
    hasSubsequence(List(1, 2, 3, 4), List(3, 4)) should be (true)
    hasSubsequence(List(1, 2, 3, 4), List(4)) should be (true)
    hasSubsequence(List(1, 2, 3, 4), List(4, 5)) should be (false)
    hasSubsequence(List(1, 2, 3, 4), List(5)) should be (false)
    hasSubsequence(List(1, 2, 3, 4), List(2, 3, 6)) should be (false)
  }

  "hasSubsequence" should "return false for second argument not Nil" in {
    hasSubsequence(Nil, List(1, 2, 3)) should be (false)
  }

  "hasSubsequence" should "return true for first and second argument equal Nil" in {
    hasSubsequence(Nil, Nil) should be (true)
  }

  "hasSubsequence" should "return true for second argument equal Nil" in {
    hasSubsequence(List(1, 2, 3), Nil) should be (true)
  }

  "addZW" should "return the sum of all arguments if they are not the same length 2" in {
    addZW(List(1, 2, 3), List(4, 5, 6, 7, 8)) should be (List(5, 7, 9, 7, 8))
  }

  "addZW" should "return the sum of all arguments if they are not the same length" in {
    addZW(List(1, 2, 3, 7, 8), List(4, 5, 6)) should be (List(5, 7, 9, 7, 8))
  }

  "addZW" should "return the sum of all arguments if they are the same length" in {
    addZW(List(1, 2, 3), List(4, 5, 6)) should be (List(5, 7, 9))
  }

  "addZW" should "return the second argument if the first argument is Nil" in {
    addZW(Nil, List(1, 2, 3)) should be (List(1, 2, 3))
  }

  "addZW" should "return the first argument if the second argument is Nil" in {
    addZW(List(1, 2, 3), Nil) should be (List(1, 2, 3))
  }

  "addZW" should "return Nil if the both arguments are Nil" in {
    addZW(Nil, Nil) should be (Nil)
  }

  "add" should "return the sum of all arguments if they are not the same length 2" in {
    add(List(1, 2, 3), List(4, 5, 6, 7, 8)) should be (List(5, 7, 9, 7, 8))
  }

  "add" should "return the sum of all arguments if they are not the same length" in {
    add(List(1, 2, 3, 7, 8), List(4, 5, 6)) should be (List(5, 7, 9, 7, 8))
  }

  "add" should "return the sum of all arguments if they are the same length" in {
    add(List(1, 2, 3), List(4, 5, 6)) should be (List(5, 7, 9))
  }

  "add" should "return the second argument if the first argument is Nil" in {
    add(Nil, List(1, 2, 3)) should be (List(1, 2, 3))
  }

  "add" should "return the first argument if the second argument is Nil" in {
    add(List(1, 2, 3), Nil) should be (List(1, 2, 3))
  }

  "add" should "return Nil if the both arguments are Nil" in {
    add(Nil, Nil) should be (Nil)
  }

  "filterFM odd numbers" should "remove all odd numbers from a list" in {
    filterFM(List(1, 2, 3, 4, 5, 6, 7))(x => x % 2 == 0) should be (List(2, 4, 6))
  }

  "filterFM" should "return Nil for empty list" in {
    filterFM(Nil)(x => true) should be (Nil)
  }

  "flatMap" should "return non empty list for an non empty list" in {
    flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  "flatMap" should "return Nil for an empty list" in {
    flatMap[Int, Int](Nil)(i => List(i, i)) should be (Nil)
  }

  "filter odd numbers" should "remove all odd numbers from a list" in {
    filter(List(1, 2, 3, 4, 5, 6, 7))(x => x % 2 == 0) should be (List(2, 4, 6))
  }

  "filter" should "return Nil for empty list" in {
    filter(Nil)(x => true) should be (Nil)
  }

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
