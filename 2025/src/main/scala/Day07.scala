package day07

import scala.io.Source

type Manifold = IndexedSeq[IndexedSeq[Char]]

@main def printAnswers: Unit = {
  val sample = parse("day07/sample.txt")

  val manifold = parse("day07/input.txt")

  println(s"Day 07 part 1: ${part1(manifold)}")
}

def parse(filepath: String): Manifold =
  Source
    .fromResource(filepath)
    .getLines()
    .map(_.toIndexedSeq)
    .toIndexedSeq

// Invariants:
// - No splitters are ever right next to each other (no instances of "^^")

// 1737 splitters
def part1(manifold: Manifold) = {
  val startingBeam = Set(manifold.head.indexOf('S'))

  // First splitter is on the 3rd row
  val (_, numSplits) = manifold.drop(2).foldLeft((startingBeam, 0)) {
    case ((beams, totalSplits), row) =>
      val newBeams = beams.toSeq.collect {
        case beam if row(beam) == '^' => Seq(beam - 1, beam + 1)
        case beam                     => Seq(beam)
      }.flatten

      val splits = newBeams.size - beams.size

      (newBeams.toSet, totalSplits + splits)
  }

  numSplits
}
