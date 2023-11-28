package day01

import scala.io.Source
import scala.util.Using

object Day01 {

  private val day01input = Using.resource(Source.fromResource("day01.txt"))(
    _.getLines().map(_.toInt).toSeq
  )

  def main(args: Array[String]) = {
    println("Part 1: " + part1(day01input))
    println("Part 2: " + part2(day01input))
  }

  def part1(input: Seq[Int]): Int =
    countIncreases(input)

  def part2(input: Seq[Int]): Int =
    countIncreases(sumByThrees(input))

  private def countIncreases(input: Seq[Int]): Int =
    (input lazyZip input.tail)
      .filter(_ < _)
      .size

  private def sumByThrees(input: Seq[Int]): Seq[Int] =
    input
      .sliding(3)
      .map(_.sum)
      .toSeq
}
