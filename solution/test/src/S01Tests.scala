package solution
import org.scalatest._
import flatspec._
import matchers._

class SolutionSpec extends AnyFlatSpec with should.Matchers {
  import Solution.*
  "last" should "work in S01" in {
    last(List(1, 1, 2, 3, 5, 8)) should be(8)
  }
  "penultimate" should "work in S02" in {
    penultimate(List(1, 1, 2, 3, 5, 8)) should be(5)
  }
  "nth" should "work in S03" in {
    nth(2, List(1, 1, 2, 3, 5, 8)) should be(2)
  }
  "length" should "work in S04" in {
    Solution.length(List(1, 1, 2, 3, 5, 8)) should be(6)
  }
  "reverse" should "work in S05" in {
    reverse(List(1, 1, 2, 3, 5, 8)) should be(List(8, 5, 3, 2, 1, 1))
  }
  "palindrome" should "work in S06" in {
    isPalindrome(List(1, 2, 3, 2, 1)) should be(true)
  }
  "flatten" should "work in S07" in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be(
      List(1, 1, 2, 3, 5, 8)
    )
  }
  "compress" should "work in S08" in {
    compress(
      List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
    ) should be(
      List("a", "b", "c", "a", "d", "e")
    )
  }
  "pack" should "work in S09" in {
    pack(
      List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
    ) should be(
      List(
        List("a", "a", "a", "a"),
        List("b"),
        List("c", "c"),
        List("a", "a"),
        List("d"),
        List("e", "e", "e", "e")
      )
    )
  }
  "encode" should "work in S10" in {
    encode(
      List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
    ) should be(
      List((4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e"))
    )
  }
}
