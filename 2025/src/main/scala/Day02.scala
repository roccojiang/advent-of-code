package day02

import scala.io.Source
import parsley.quick.*
import parsley.debug, debug.*

@main def printAnswers(): Unit = {
  val ranges = parse("day02/input.txt")

  println(s"Day 02 part 1: ${part1(ranges)}")
  println(s"Day 02 part 2: ${part2(ranges)}")
}

def parse(filepath: String): Seq[String] =
  Source
    .fromResource(filepath)
    .getLines()
    .flatMap(_.split(','))
    .flatMap { s =>
      val Array(start, end) = s.split('-')
      (start.toLong to end.toLong).map(_.toString)
    }
    .toSeq

// didn't want to use regexes tbh, but raw"(\d+)\1+".r would do the trick (remove final '+' for part 1)
def isRepeated(id: String, repeats: Int = 2): Boolean = {
  if (id.size % repeats != 0) false
  else {
    val splits = id.grouped(id.size / repeats).toSeq
    splits.forall(_ == splits.head)
  }
}

def part1(ids: Seq[String]): Long =
  ids
    .filter(isRepeated(_))
    .map(_.toLong)
    .sum

// brute force lmao
def part2(ids: Seq[String]): Long =
  ids
    .filter { id => (2 to id.size).exists(isRepeated(id, _)) }
    .map(_.toLong)
    .sum
