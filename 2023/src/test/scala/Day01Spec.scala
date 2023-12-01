package day01

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.io.Source

class Day01Spec extends AnyFlatSpec {
  "Part 1" should "work for sample input" in {
    val input = Day01.parse("day01/sample1.txt")
    Day01.part1(input) shouldBe 142
  }

  "Part 2" should "work for sample input" in {
    val input = Day01.parse("day01/sample2.txt")
    Day01.part2(input) shouldBe 281
  }
}
