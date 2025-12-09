package day09

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class Day09Spec extends AnyFlatSpec {
  val redTiles = parse("day09/sample.txt")

  "Part 1" should "find the largest area of any possible rectangle" in {
    part1(redTiles) shouldBe 50
  }

  "Part 2" should "find the largest area of any rectangle made only of red and green tiles" in {
    part2(redTiles) shouldBe 24
  }
}
