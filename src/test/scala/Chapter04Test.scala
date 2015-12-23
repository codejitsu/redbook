import org.scalatest.{FlatSpec, Matchers}
import redbook.Chapter04._

class Chapter04Test extends FlatSpec with Matchers {
  "filter" should "retrun None for None" in {
    None.filter(_ => true) should be (None)
  }

  "filter" should "return the same Some if value satisfies f" in {
    Some(1).filter(_ == 1) should be (Some(1))
  }

  "filter" should "convert Some to None if value does not satisfy f" in {
    Some(1).filter(_ > 10) should be (None)
  }

  "orElse" should "handle Some" in {
    Some(1).orElse(Some(42)) should be (Some(1))
  }

  "orElse" should "handle None" in {
    None.orElse(Some(42)) should be (Some(42))
  }

  "getOrElse" should "handle Some" in {
    Some(1).getOrElse(42) should be (1)
  }

  "getOrElse" should "handle None" in {
    None.getOrElse(42) should be (42)
  }

  "flatMap" should "handle Some" in {
    Some(42).flatMap(x => Some(x.toString)) should be (Some("42"))
  }

  "flatMap" should "handle None" in {
    None.flatMap(_ => Some(100)) should be (None)
  }

  "map" should "handle Some" in {
    Some(42).map(_.toString) should be (Some("42"))
  }

  "map" should "handle None" in {
    None.map(_ => 100) should be (None)
  }
}
