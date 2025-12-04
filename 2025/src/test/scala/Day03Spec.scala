package day03

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Day03Spec extends AnyFlatSpec {
  val sampleInput = parse("day03/sample.txt")

  "Part 1" should "calculate the correct maximum joltages using two batteries" in {
    sampleInput.map(maxJoltage(_, 2)) shouldBe Seq(98, 89, 78, 92)
  }

  "Part 2" should "calculate the correct maximum joltages using 12 batteries" in {
    sampleInput.map(maxJoltage(_, 12)) shouldBe Seq(
      987654321111L,
      811111111119L,
      434234234278L,
      888911112111L
    )
  }
}
