package day05

import scala.io.Source
import scala.collection.immutable.NumericRange

@main def printAnswers(): Unit = {
  val (freshRanges, ingredients) = parse("day05/input.txt")

  println(s"Day 05 part 1: ${part1(freshRanges, ingredients)}")
  println(s"Day 05 part 2: ${part2(freshRanges)}")
}

def parse(filepath: String): (Set[LongRange], Seq[Long]) = {
  val (ranges, ids) = Source
    .fromResource(filepath)
    .getLines()
    .span(_.nonEmpty)

  val freshRanges = ranges.map { s =>
    s match
      case s"$l-$h" => LongRange(l.toLong, h.toLong)
  }.toSet

  (freshRanges, ids.toSeq.tail.map(_.toLong))
}

def part1(freshRanges: Set[LongRange], ids: Seq[Long]): Long =
  ids.count { id => freshRanges.exists(_ contains id) }

def part2(freshRanges: Set[LongRange]): Long =
  freshRanges
    .foldLeft(Set.empty[LongRange])(_ addRange _)
    .iterator // avoid Set from deduplicating ranges of the same size!
    .map(_.size)
    .sum

case class LongRange(start: Long, end: Long) {
  infix def contains(n: Long): Boolean = start <= n && n <= end

  // Alternatively, if we used (start <= other.end + 1) && (end + 1 >= other.start) for the 'intersects' check in addRange,
  // it would help merge adjacent ranges which border each other, but do not intersect
  infix def intersects(other: LongRange): Boolean =
    start <= other.end && end >= other.start

  infix def union(other: LongRange): Option[LongRange] =
    Option.when(this intersects other)(
      LongRange(start min other.start, end max other.end)
    )

  lazy val size: Long = end - start + 1
}

extension (rs: Set[LongRange]) {
  @annotation.tailrec
  infix def addRange(r: LongRange): Set[LongRange] =
    rs.find(_ intersects r) match
      case None              => rs + r
      case Some(overlapping) =>
        (rs - overlapping) addRange (r union overlapping).get
}
