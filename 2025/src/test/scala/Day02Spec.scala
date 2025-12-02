package day02

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Day02Spec extends AnyFlatSpec {
  "Part 1" should "correctly sum up all invalid IDs" in {
    part1(parse("day02/sample.txt")) shouldBe 1227775554
  }
}
