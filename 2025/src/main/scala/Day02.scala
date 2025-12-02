package day02

import scala.io.Source
import parsley.quick.*
import parsley.debug, debug.*

@main def printAnswers(): Unit = {
  val ranges = parse("day02/input.txt")

  println(s"Day 02 part 1: ${part1(ranges)}")
  // println(s"Day 02 part 2: ${part2(rotations)}")
}

def parse(filepath: String) =
  Source
    .fromResource(filepath)
    .getLines()
    .flatMap(_.split(','))
    .flatMap { s =>
      val Array(start, end) = s.split('-')
      (start.toLong to end.toLong).map(_.toString)
    }

def invalidId(id: String): Boolean = {
  val (first, second) = id.splitAt(id.size / 2)
  first == second
}

def part1(ids: Iterator[String]): Long = {
  ids.filter(invalidId).map(_.toLong).sum
}
