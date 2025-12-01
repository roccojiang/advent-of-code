package day01

import scala.io.Source

object Day01 {

  type Degrees = Int
  type Rotation = Int

  val DialStartPosition: Degrees = 50

  def parse(filepath: String): Seq[String] = Source.fromResource(filepath).getLines().toSeq

  def convertRotation(str: String): Rotation = {
    val (rotation, degrees) = str.splitAt(1)
    rotation match {
      case "R" => degrees.toInt
      case "L" => -degrees.toInt
    }
  }

  def rotate(dial: Degrees, rotation: Rotation): Degrees = {
    val rotated = (dial + rotation) % 100
    rotated + (if rotated < 0 then 100 else 0)
  }

  def rotateSequence(rotations: Seq[Rotation]): Seq[Degrees] = rotations.scanLeft(DialStartPosition)(rotate(_, _))

  def part1(lines: Seq[String]): Int = rotateSequence(lines.map(convertRotation(_))).count(_ == 0)

  @main def printAnswers(): Unit = {
    val lines = parse("day01/input.txt")

    println(s"Day 01 part 1: ${part1(lines)}")
  }
}
