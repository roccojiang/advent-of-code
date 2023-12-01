package day01

import scala.io.Source

object Day01 {
  val numberMap = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  def parse(filepath: String): Seq[String] = Source.fromResource(filepath).getLines().toSeq

  // Extract digit from beginning of the string, based on rules from Part 2
  def extractDigit(slice: String): Option[Int] = {
    if (slice.head.isDigit) {
      Some(slice.head.asDigit)
    } else {
      numberMap.keys.find(slice.startsWith).flatMap(numberMap.get)
    }
  }

  // Extract all digits within a string, based on rules from Part 2
  def extractAllDigits(line: String): Seq[Int] = {
    if (line.isEmpty) {
      Seq.empty
    } else {
      extractDigit(line).toSeq ++ extractAllDigits(line.drop(1))
    }
  }

  def combineDigits(digits: Seq[Int]) = {
    assert (digits.length >= 1)
    digits.head * 10 + digits.last
  }

  def part1(lines: Seq[String]): Int = lines.map(line => combineDigits(line.filter(_.isDigit).map(_.asDigit))).sum
  def part2(lines: Seq[String]): Int = lines.map(line => combineDigits(extractAllDigits(line))).sum

  def main(args: Array[String]): Unit = {
    val lines = parse("day01/input.txt")

    println("Day 01 part 1: " + part1(lines))
    println("Day 01 part 2: " + part2(lines))
  }
}
