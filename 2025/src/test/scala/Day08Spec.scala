package day08

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Day08Spec extends AnyFlatSpec {
  val positions = parse("day08/sample.txt")

  "Part 1" should "multiply the sizes of the largest three circuits after making 10 connections" in {
    // NOTE: In the problem example using the sample input, it only asks to make 20/2 = 10 connections,
    // but in the actual problem it asks to make all 1000 connections!
    part1(positions, 10) shouldBe 40
  }
}
