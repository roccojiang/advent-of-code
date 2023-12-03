package day03

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import parsley.Success

class Day03Spec extends AnyFlatSpec {
  "Parser" should "parse numbers and symbols with their positions correctly" in {
    val input = "...123..@..34..\n..#..74..%"
    Day03.parser.schematic.parse(input) shouldBe Success(List(List((Left(123),(0,5)), (Right('@'),(0,8)), (Left(34),(0,12))), List((Right('#'),(1,2)), (Left(74),(1,6)), (Right('%'),(1,9)))))
  }

  "Part 1" should "work for sample input" in {
    val input = Day03.parse("day03/sample.txt")
    Day03.part1(input) shouldBe 4361
  }

  it should "work for further sample input" in {
    val input = Day03.parse("day03/more_exhaustive_sample.txt")
    Day03.part1(input) shouldBe 925
  }

  "Part 2" should "work for sample input" in {
    val input = Day03.parse("day03/sample.txt")
    Day03.part2(input) shouldBe 467835
  }

  it should "work for further sample input" in {
    val input = Day03.parse("day03/more_exhaustive_sample.txt")
    Day03.part2(input) shouldBe 6756
  }
}
