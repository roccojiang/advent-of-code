package day06

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Day06Spec extends AnyFlatSpec {
  val sampleLines = parseLines("day06/sample.txt")

  "Part 1" should "calculate the correct total based on standard parsing rules" in {
    part1(sampleLines) shouldBe 4277556L
  }

  "Part 2" should "calculate the correct total based on column-wise parsing rules" in {
    part2(sampleLines) shouldBe 3263827L
  }
}
