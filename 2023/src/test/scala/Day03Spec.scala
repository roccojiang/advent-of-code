package day03

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Success

import Day03.Coord

class Day03Spec extends AnyFlatSpec {
  "Parser" should "parse numbers and symbols with their positions correctly" in {
    val input = "...123..@..34..\n..#..74..%"
    Day03.parser.schematic.parse(input) shouldBe Success(List(List((Left(123),Coord(5,0)), (Right('@'),Coord(8,0)), (Left(34),Coord(12,0))), List((Right('#'),Coord(2,1)), (Left(74),Coord(6,1)), (Right('%'),Coord(9,1)))))
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
