package day01

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Day01Spec extends AnyFlatSpec {
  "Part 1" should "calculate the correct rotations" in {
    val input = Seq("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")
    Day01.rotateSequence(input.map(Day01.convertRotation(_))) shouldBe Seq(50, 82, 52, 0, 95, 55, 0, 99, 0, 14, 32)
  }

  it should "count the correct times the dial points at zero" in {
    val input = Seq("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")
    Day01.part1(input) shouldBe 3
  }

  it should "never have the dial point outside its correct range" in {
    val input = Day01.parse("day01/input.txt")
    Day01.rotateSequence(input.map(Day01.convertRotation(_))).count(d => !(0 to 99 contains d)) shouldBe 0
  }
}
