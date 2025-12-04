package day03

import scala.io.Source

@main def printAnswers(): Unit = {
  val banks = parse("day03/input.txt")

  println(s"Day 03 part 1: ${part1(banks)}")
  println(s"Day 03 part 2: ${part2(banks)}")
}

def parse(filepath: String): Seq[Seq[Int]] =
  Source
    .fromResource(filepath)
    .getLines()
    .map(_.map(_.asDigit))
    .toSeq

def maxJoltage(bank: Seq[Int], batteriesToTurnOn: Int): Long = {
  @annotation.tailrec
  def loop(batteries: Seq[Int], count: Int, acc: Long): Long = {
    if count == 0 then acc
    else {
      val (maxBattery, maxBatteryIdx) =
        batteries
          .dropRight(count - 1)
          .zipWithIndex
          .maxBy(_(0))

      loop(
        batteries.drop(maxBatteryIdx + 1),
        count - 1,
        acc * 10 + maxBattery
      )
    }
  }

  loop(bank, batteriesToTurnOn, 0)
}

def part1(banks: Seq[Seq[Int]]) = banks.map(maxJoltage(_, 2)).sum
def part2(banks: Seq[Seq[Int]]) = banks.map(maxJoltage(_, 12)).sum
