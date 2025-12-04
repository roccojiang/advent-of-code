package day04

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Day04Spec extends AnyFlatSpec {
  val sampleGrid = parseGrid("day04/sample.txt")

  "Part 1" should "count the correct number of accessible rolls" in {
    part1(sampleGrid) shouldBe 13
  }

  "Part 2" should "count the correct number of accessible rolls in total" in {
    part2(sampleGrid) shouldBe 43
  }
}
