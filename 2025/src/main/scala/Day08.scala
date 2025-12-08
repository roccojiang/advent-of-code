package day08

import scala.collection.mutable
import scala.io.Source

@main def printAnswers = {
  val positions = parse("day08/input.txt")

  // NOTE: the problem asks to make ALL 1000 pairwise connections,
  // rather than the 500 that the example would otherwise suggest
  println(s"Day 08 part 1: ${part1(positions, 1000)}")
}

def parse(filepath: String): Seq[Pos] =
  Source
    .fromResource(filepath)
    .getLines()
    .map(_.toPos)
    .toSeq

def part1(boxes: Seq[Pos], pairsToConnect: Int): Int = {
  val connectedPairs =
    boxes
      .combinations(2)
      .toSeq
      .sortBy { case Seq(a, b) => a.squareDistance(b) }
      .take(pairsToConnect)
      .map(_.toSet)

  // Join the connected pairs first
  val circuits = connectedPairs.foldLeft(DisjointSet.empty[Pos])(_ ++ _)
  // Add all individual junction boxes back
  val allCircuits = boxes.foldLeft(circuits)(_ + _)

  allCircuits.toSet.toSeq
    .map(_.size)
    .sorted(using Ordering[Int].reverse)
    .take(3)
    .product
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
  def empty[A] = new DisjointSet(Set.empty[Set[A]])
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
