package day09

import scala.io.Source

@main def printAnswers = {
  val redTiles = parse("day09/input.txt")

  println(s"Day 09 part 1: ${part1(redTiles)}")
  println(s"Day 09 part 2: ${part2(redTiles)}")
}

def parse(filepath: String): Seq[Pos] =
  Source
    .fromResource(filepath)
    .getLines()
    .map(_.toPos)
    .toSeq

def allRectangles(tiles: Seq[Pos]): Seq[Rect] =
  tiles
    .combinations(2)
    .map { case Seq(p1, p2) => Rect(p1, p2) }
    .toSeq

def part1(redTiles: Seq[Pos]): Long = allRectangles(redTiles).map(_.area).max

def contained(rect: Rect, tiles: Seq[Pos]): Boolean = {
  val polygonLines = tiles.zip(tiles.tail :+ tiles.head).map(Line.apply)
  polygonLines.forall { case Line(p1, p2) =>
    (p1.x max p2.x) <= (rect.p1.x min rect.p2.x) || // line falls to the left of the leftmost rectangle edge
    (p1.x min p2.x) >= (rect.p1.x max rect.p2.x) || // line falls to the right of the rightmost rectangle edge
    (p1.y max p2.y) <= (rect.p1.y min rect.p2.y) || // line falls above the topmost rectangle edge
    (p1.y min p2.y) >= (rect.p1.y max rect.p2.y) // line falls below the bottommost rectangle edge
  }
}

def part2(redTiles: Seq[Pos]): Long =
  allRectangles(redTiles).collect {
    case rect if contained(rect, redTiles) => rect.area
  }.max

case class Pos(x: Int, y: Int)

case class Line(p1: Pos, p2: Pos) {
  require(p1.x == p2.x || p1.y == p2.y)
}

case class Rect(p1: Pos, p2: Pos) {
  def area: Long =
    (math.abs(p1.x - p2.x) + 1).toLong * (math.abs(p1.y - p2.y) + 1).toLong
}

extension (s: String)
  def toPos: Pos = s match {
    case s"$x,$y" => Pos(x.toInt, y.toInt)
  }
