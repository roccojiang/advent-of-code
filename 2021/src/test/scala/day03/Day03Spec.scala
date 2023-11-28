package day03

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Spec extends AnyFlatSpec with Matchers {

  val sampleInput = Seq(
    Seq(0, 0, 1, 0, 0),
    Seq(1, 1, 1, 1, 0),
    Seq(1, 0, 1, 1, 0),
    Seq(1, 0, 1, 1, 1),
    Seq(1, 0, 1, 0, 1),
    Seq(0, 1, 1, 1, 1),
    Seq(0, 0, 1, 1, 1),
    Seq(1, 1, 1, 0, 0),
    Seq(1, 0, 0, 0, 0),
    Seq(1, 1, 0, 0, 1),
    Seq(0, 0, 0, 1, 0),
    Seq(0, 1, 0, 1, 0)
  )

  "Part 1" should "multiply the gamma rate and epsilon rate" in {
    Day03.part1(sampleInput) shouldEqual 198
  }

  "Part 2" should "multiply the oxygen generator rating and CO2 scrubber rating" in {
    Day03.part2(sampleInput) shouldEqual 230
  }
}
