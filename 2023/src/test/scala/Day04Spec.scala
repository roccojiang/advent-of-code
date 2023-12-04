package day04

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Day04Spec extends AnyFlatSpec {
  "Part 1" should "work for sample input" in {
    val input = Day04.parse("Day04/sample.txt")
    Day04.part1(input) shouldBe 13
  }

  "Part 2" should "work for sample input" in {
    val input = Day04.parse("Day04/sample.txt")
    Day04.part2(input) shouldBe 30
  }
}
