package day01

import scala.io.Source

@main def printAnswers(): Unit = {
  val rotations = parse("day01/input.txt")

  println(s"Day 01 part 1: ${part1(rotations)}")
  println(s"Day 01 part 2: ${part2(rotations)}")
}

type Degrees = Int
type Rotation = Int

val DialStartPosition: Degrees = 50
val DialSize: Int = 100

def parse(filepath: String): Seq[Rotation] =
  Source
    .fromResource(filepath)
    .getLines()
    .map(parseRotation)
    .toSeq

def parse(lines: Seq[String]): Seq[Rotation] = lines.map(parseRotation)

def parseRotation(s: String): Rotation = s match {
  case s"L$distance" => -distance.toInt
  case s"R$distance" => distance.toInt
}

case class Dial(position: Degrees = DialStartPosition, passes: Int = 0) {
  def rotate(rotation: Rotation): Dial = {
    val newPosition = (position + rotation) mod DialSize

    val distanceToNextZero =
      if position == 0 then DialSize
      else if rotation >= 0 then DialSize - position
      else position
    val fullRotations = (rotation.abs - distanceToNextZero) / DialSize
    val passesZeroAtLeastOnce = rotation.abs >= distanceToNextZero
    val newPasses =
      passes + (if passesZeroAtLeastOnce then fullRotations + 1 else 0)

    Dial(newPosition, newPasses)
  }
}

def rotateSequence(rotations: Seq[Rotation]): Seq[Dial] =
  rotations.scanLeft(Dial())(_.rotate(_))

def part1(rotations: Seq[Rotation]): Int =
  rotateSequence(rotations).count(_.position == 0)

def part2(rotations: Seq[Rotation]): Int =
  rotations
    .foldLeft(Dial())(_.rotate(_))
    .passes

extension (n: Int) {
  infix def mod(d: Int): Int = math.floorMod(n, d)
  infix def divMod(d: Int): (Int, Int) =
    (math.floorDiv(n, d), math.floorMod(n, d))
}
