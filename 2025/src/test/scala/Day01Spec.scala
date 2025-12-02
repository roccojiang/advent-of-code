package day01

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Day01Spec extends AnyFlatSpec {
  val sampleInput =
    Seq("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")

  "Part 1" should "calculate the correct rotations" in {
    rotateSequence(parse(sampleInput)).map(_.position) shouldBe Seq(50, 82, 52,
      0, 95, 55, 0, 99, 0, 14, 32)
  }

  it should "count the correct times the dial points at zero" in {
    part1(parse(sampleInput)) shouldBe 3
  }

  it should "never have the dial point outside its correct range" in {
    val input = parse("day01/input.txt")
    rotateSequence(input).count(d => !(0 to 99 contains d.position)) shouldBe 0
  }

  "Part 2" should "fully track the correct times the dial passes zero" in {
    rotateSequence(parse(sampleInput)) shouldBe Seq(
      Dial(50, 0),
      Dial(82, 1),
      Dial(52, 1),
      Dial(0, 2),
      Dial(95, 2),
      Dial(55, 3),
      Dial(0, 4),
      Dial(99, 4),
      Dial(0, 5),
      Dial(14, 5),
      Dial(32, 6)
    )
  }

  it should "count the correct number of times the dial passes zero" in {
    part2(parse(sampleInput)) shouldBe 6
  }
}
