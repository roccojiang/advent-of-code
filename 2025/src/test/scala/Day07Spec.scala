package day07

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Day07Spec extends AnyFlatSpec {
  val manifold = parse("day07/sample.txt")

  "Part 1" should "count the number of times the beam will split" in {
    part1(manifold) shouldBe 21
  }
}
