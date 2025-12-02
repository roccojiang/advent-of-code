package day02

import scala.io.Source

@main def printAnswers(): Unit = {
  val ranges = parse("day02/input.txt")
  println(ranges.toSeq)

  // println(s"Day 01 part 1: ${part1(rotations)}")
  // println(s"Day 02 part 2: ${part2(rotations)}")
}

def parse(filepath: String) =
  Source
    .fromResource(filepath)
    .getLines()
    .flatMap(_.split(','))
    .map { s =>
      val Array(start, end) = s.split('-')
      start.toLong to end.toLong
    }
