package day06

import scala.io.Source

@main def printAnswers: Unit = {
  val columns = parseColumns("day06/input.txt")

  println(s"Day 06 part 1: ${part1(columns)}")
}

def parseColumns(filepath: String): Seq[Seq[String]] =
  Source
    .fromResource(filepath)
    .getLines()
    .map(_.split(" ").filterNot(_ == ""))
    .toSeq
    .transpose

def processColumn(col: Seq[String]): Long = {
  val op: (Long, Long) => Long = col.last match {
    case "+" => _ + _
    case "*" => _ * _
  }

  col.init.map(_.toLong).reduce(op)
}

def part1(cols: Seq[Seq[String]]): Long = cols.map(processColumn).sum
