package redbook

import org.scalatest.{Matchers, FlatSpec}
import Chapter02._

class Chapter02Test extends FlatSpec with Matchers {
  "fib(1)" should "be equal 0" in {
    fib(1) should be (0)
  }

  "fib(2)" should "be equal 1" in {
    fib(2) should be (1)
  }

  "fib(3)" should "be equal 1" in {
    fib(3) should be (1)
  }

  "fib(6)" should "be equal 5" in {
    fib(6) should be (5)
  }

  "fib(16)" should "be equal 610" in {
    fib(16) should be (610)
  }

  "fib(29)" should "be equal 317811" in {
    fib(29) should be (317811)
  }

  "isSorted" should "return true for empty array" in {
    isSorted(Array.empty[Int], (a: Int, b: Int) => a <= b) should be (true)
  }

  "isSorted" should "return true for one element array" in {
    isSorted(Array(1), (a: Int, b: Int) => a <= b) should be (true)
  }

  "isSorted" should "return true for sorted array" in {
    isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a <= b) should be (true)
    isSorted(Array(1, 2, 3, 4, 5, 6), (a: Int, b: Int) => a <= b) should be (true)
  }

  "isSorted" should "return false for unsorted array" in {
    isSorted(Array(1, 2, 3, 5, 4), (a: Int, b: Int) => a <= b) should be (false)
    isSorted(Array(1, 3, 2, 4, 5, 6), (a: Int, b: Int) => a <= b) should be (false)
  }

  "curry" should "convert a 2-argument function into 1-argument function" in {
    def fun(x: Int, y: Int): Int = x + y

    val curried = curry(fun)(5)

    curried(10) should be (15)
    curried(2) should be (7)
  }

  "uncurry" should "reverse curry" in {
    def fun(x: Int, y: Int): Int = x + y

    val curried = curry(fun)

    curried(5)(10) should be (15)
    curried(5)(2) should be (7)

    val uncurried = uncurry(curried)

    uncurried(1, 2) should be (3)
  }

  "compose" should "compose two functions" in {
    def one(x: String): Int = x.length

    def two(a: Array[Char]): String = a.mkString

    val comp = compose(one, two)

    val arr = Array('a', 'b', 'c')

    comp(arr) should be (one(two(arr)))
  }
}
