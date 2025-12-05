package day05

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Day05Spec extends AnyFlatSpec {
  val (freshRanges, ids) = parse("day05/sample.txt")

  "Part 1" should "count the fresh ingredients" in {
    part1(freshRanges, ids) shouldBe 3
  }

  "Part 2" should "count the total fresh ingredient IDs" in {
    part2(freshRanges) shouldBe 14
  }
}
