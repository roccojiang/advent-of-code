package day02

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Day02Spec extends AnyFlatSpec {
  "Part 1" should "work for sample input" in {
    val input = Day02.parse("day02/sample.txt")
    Day02.part1(input) shouldBe 8
  }

  "Part 2" should "work for sample input" in {
    val input = Day02.parse("day02/sample.txt")
    Day02.part2(input) shouldBe 2286
  }
}
