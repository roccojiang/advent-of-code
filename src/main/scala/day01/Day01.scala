package day01

import scala.io.Source

object Day01 {

  val day01input = readInput("day01.txt")

  def readInput(filename: String): Seq[Int] = {
    val bufferedSource = Source.fromResource(filename)
    val lines = bufferedSource.getLines().map(_.toInt).toSeq
    bufferedSource.close
    lines
  }

  def countIncreases(input: Seq[Int]): Int =
    (input.drop(1) lazyZip input).filter(_ > _).length

  object Part1 {
    def solve(input: Seq[Int]): Int =
      countIncreases(input)
  }

  object Part2 {
    def sumByThrees(input: Seq[Int]): Seq[Int] =
      (input.drop(2) lazyZip input.drop(1) lazyZip input).map(_ + _ + _)

    def solve(input: Seq[Int]): Int =
      countIncreases(sumByThrees(input))
  }

  def main(args: Array[String]) = {
    println("Part 1: " + Part1.solve(day01input))
    println("Part 2: " + Part2.solve(day01input))
  }
}
