package day08

import scala.collection.mutable
import scala.io.Source

@main def printAnswers = {
  val positions = parse("day08/input.txt")

  // NOTE: the problem asks to make ALL 1000 pairwise connections,
  // rather than the 500 that the example would otherwise suggest
  println(s"Day 08 part 1: ${part1(positions, 1000)}")

  println(s"Day 08 part 2: ${part2(positions)}")
}

def parse(filepath: String): Seq[Pos] =
  Source
    .fromResource(filepath)
    .getLines()
    .map(_.toPos)
    .toSeq

def getConnectedPairs(boxes: Seq[Pos]): Seq[Set[Pos]] =
  boxes
    .combinations(2)
    .toSeq
    .sortBy { case Seq(a, b) => a.squareDistance(b) }
    .map(_.toSet)

def part1(boxes: Seq[Pos], pairsToConnect: Int): Int = {
  val circuits =
    getConnectedPairs(boxes)
      .take(pairsToConnect)
      .foldLeft(DisjointSet(boxes.map(Set(_)).toSet))(_ ++ _)

  circuits.toSet.toSeq
    .map(_.size)
    .sorted(using Ordering[Int].reverse)
    .take(3)
    .product
}

def part2(boxes: Seq[Pos]): Long = {
  @annotation.tailrec
  def loop(pairs: Seq[Set[Pos]], circuits: DisjointSet[Pos]): (Pos, Pos) = {
    val (pair, rest) = (pairs.head, pairs.tail) // unsafe, but the problem guarantees termination before we run out of pairs
    val newCircuits = circuits ++ pair
    if newCircuits.toSet.map(_.size) contains boxes.size then
      val Seq(a, b) = pair.toSeq
      (a, b)
    else loop(rest, newCircuits)
  }

  val (a, b) = loop(getConnectedPairs(boxes), DisjointSet(boxes.map(Set(_)).toSet))
  a.x * b.x
}

final class DisjointSet[A] private (private val entries: Set[Set[A]]) {
  infix def +(elem: A): DisjointSet[A] = entries.find(_ contains elem) match {
    case None           => DisjointSet(entries + Set(elem))
    case Some(existing) => DisjointSet((entries - existing) + (existing + elem))
  }

  @annotation.tailrec
  infix def ++(entry: Set[A]): DisjointSet[A] =
    entries.find(_.intersect(entry).nonEmpty) match {
      case None           => DisjointSet(entries + entry)
      case Some(existing) =>
        // Recursive case: new set needs to be recursively merged with any other existing sets
        DisjointSet(entries - existing) ++ (existing union entry)
    }

  def toSet: Set[Set[A]] = entries
}

object DisjointSet {
  def apply[A](entries: Set[Set[A]]): DisjointSet[A] =
    new DisjointSet(entries)

  def empty[A]: DisjointSet[A] =
    new DisjointSet(Set.empty[Set[A]])
}

case class Pos(x: Long, y: Long, z: Long) {
  def squareDistance(other: Pos): Long = {
    val dx = x - other.x
    val dy = y - other.y
    val dz = z - other.z

    dx * dx + dy * dy + dz * dz
  }
}

extension (s: String)
  def toPos = {
    val Array(x, y, z) = s.split(',')
    Pos(x.toInt, y.toInt, z.toInt)
  }
