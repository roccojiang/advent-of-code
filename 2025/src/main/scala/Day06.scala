package day06

import scala.io.Source

@main def printAnswers: Unit = {
  val lines = parseLines("day06/input.txt")

  println(s"Day 06 part 1: ${part1(lines)}")
  println(s"Day 06 part 2: ${part2(lines)}")
}

def op(c: String): (Long, Long) => Long = c match {
  case "+" => _ + _
  case "*" => _ * _
}

def parseLines(filepath: String): Seq[String] =
  Source.fromResource(filepath).getLines().toSeq

def part1(lines: Seq[String]): Long = {
  val cols = lines.map { _.split(" ").filterNot(_ == "") }.transpose

  cols.map { col =>
    col.init
      .map(_.toLong)
      .reduce(op(col.last))
  }.sum
}

def part2(lines: Seq[String]): Long = {
  val ops = lines.last.split(" +").map(op)
  val problems = lines.init.transpose
    .map(l => l.mkString.trim)
    .splitOn("")
    .map(_.map(_.toLong))

  problems.zip(ops).map { (nums, op) => nums.reduce(op) }.sum
}

extension [A](xs: Iterable[A]) {
  def splitOn(sep: A): Iterable[Iterable[A]] = {
    val (res, cur) = (Seq.newBuilder[Seq[A]], Seq.newBuilder[A])
    for x <- xs do
      if x != sep then cur += x
      else
        res += cur.result()
        cur.clear()
    (res += cur.result()).result()
  }
}
