package day07

import scala.io.Source

type Manifold = IndexedSeq[IndexedSeq[Char]]
type BeamLocation = Int

@main def printAnswers: Unit = {
  val manifold = parse("day07/input.txt")

  println(s"Day 07 part 1: ${part1(manifold)}")
  println(s"Day 07 part 2: ${part2(manifold)}")
}

def parse(filepath: String): Manifold =
  Source
    .fromResource(filepath)
    .getLines()
    .map(_.toIndexedSeq)
    .toIndexedSeq

// Pre: no splitters are ever right next to each other (no instances of "^^")
def simulateTachyon(manifold: Manifold): (Map[BeamLocation, Long], Int) = {
  val startingBeams = Map(manifold.head.indexOf('S') -> 1L).withDefaultValue(0L)

  // First splitter is on the 3rd row
  manifold.drop(2).foldLeft((startingBeams, 0)) {
    case ((beams, totalSplits), row) =>
      val hitsSplitter: (Int => Boolean) = beam => row(beam) == '^'

      val newBeams = beams.foldLeft(beams) {
        case (acc, (beam, c)) if hitsSplitter(beam) =>
          acc
            .removed(beam)
            .updated(beam - 1, acc(beam - 1) + c)
            .updated(beam + 1, acc(beam + 1) + c)
        case (acc, _) => acc
      }

      val splits = beams.keysIterator.count(hitsSplitter)

      (newBeams, totalSplits + splits)
  }
}

def part1(manifold: Manifold): Int = {
  val (_, numSplits) = simulateTachyon(manifold)
  numSplits
}

def part2(manifold: Manifold) = {
  val (beams, _) = simulateTachyon(manifold)
  beams.valuesIterator.sum
}
