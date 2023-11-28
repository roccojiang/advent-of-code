package day01

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Spec extends AnyFlatSpec with Matchers {

  val sampleInput = Seq(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  "Part 1" should "count the number of times a measurement increases from the previous" in {
    Day01.part1(sampleInput) shouldEqual 7
  }

  "Part 2" should "count the number of times the sum of measurements in the sliding window increases" in {
    Day01.part2(sampleInput) shouldEqual 5
  }
}
