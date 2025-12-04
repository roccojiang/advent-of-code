package day04

import scala.io.Source

@main def printAnswers(): Unit = {
  val grid = parseGrid("day04/input.txt")

  println(s"Day 04 part 1: ${part1(grid)}")
  println(s"Day 04 part 2: ${part2(grid)}")
}

type Grid[T] = Vector[Vector[T]]

case class Pos(x: Int, y: Int) {
  lazy val neighbours: Set[Pos] = (for {
    dx <- -1 to 1
    dy <- -1 to 1
  } yield Pos(x + dx, y + dy)).toSet - this
}

def parseGrid(filepath: String): Grid[Char] =
  Source
    .fromResource(filepath)
    .getLines()
    .map(_.toVector)
    .toVector

def getRollCoords(grid: Grid[Char]): Set[Pos] =
  (for {
    (row, y) <- grid.zipWithIndex
    (cell, x) <- row.zipWithIndex
    if cell == '@'
  } yield Pos(x, y)).toSet

// Neat thing about storing a Set[Pos] is that I don't have to worry about bounds checking
def partitionAccessibleRolls(rolls: Set[Pos]): (Set[Pos], Set[Pos]) =
  rolls.partition(_.neighbours.count(rolls) < 4)

def part1(grid: Grid[Char]): Int = {
  val (accessible, _) = partitionAccessibleRolls(getRollCoords(grid))
  accessible.size
}

def part2(grid: Grid[Char]): Int =
  Iterator
    .unfold(getRollCoords(grid)) { rolls =>
      val (accessible, remaining) = partitionAccessibleRolls(rolls)

      if accessible.isEmpty then None
      else Some(accessible.size, remaining)
    }
    .sum
