package day02

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Spec extends AnyFlatSpec with Matchers {

  val sampleInput = Seq(
    ("forward", 5),
    ("down", 5),
    ("forward", 8),
    ("up", 3),
    ("down", 8),
    ("forward", 2)
  )

  "Part 1" should "multiply the final horizontal position by the final depth" in {
    Day02.part1(sampleInput) shouldEqual 150
  }

  "Part 2" should "multiply the final horizontal position by the final depth, interpreting commands by tracking aim" in {
    Day02.part2(sampleInput) shouldEqual 900
  }
}
