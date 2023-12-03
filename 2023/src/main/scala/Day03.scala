package day03

import parsley.Parsley
import parsley.character._
import parsley.combinator._
import parsley.position._
import parsley.implicits.character.stringLift

import scala.io.Source

object Day03 {
  case class Coord(x: Int, y: Int)
  case class Bound(left: Coord, right: Coord)

  type Index = Int
  type IndexedInt = (Int, Index)

  type Schematic = List[(Either[Int, Char], Coord)]

  object parser {
    val number = digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)
    val symbol = satisfy(c => !(c.isDigit || c == '.' || c.isWhitespace))

    val numOrSymbol = (number | symbol).map {
      case n: Int => Left(n)
      case c: Char => Right(c)
    }

    // Correct for the fact that Parsley's position is 1-indexed
    // Column value is also adjusted by one, to index the last character matched in the number/symbol
    val position = pos.map {
      case (row, col) => Coord(col - 1 - 1, row - 1)
    }

    val row = many(".") ~> many(numOrSymbol <~> position <~ many("."))
    val schematic = sepBy(row, newline)
  }

  def digits(n: Int) = if (n == 0) 1 else math.log10(math.abs(n)).toInt + 1;

  def parse(filepath: String): Schematic = {
    val input = Source.fromResource(filepath).getLines().toSeq.mkString("\n")
    parser.schematic.parse(input).get.flatten
  }

  def getSymbolsAroundBound(bound: Bound, symbolsMap: Map[Coord, Char]): Seq[Char] = {
    val foundSymbols = for {
      x <- bound.left.x - 1 to bound.right.x + 1
      y <- bound.left.y - 1 to bound.right.y + 1
    } yield symbolsMap.get(Coord(x, y))

    foundSymbols.filter(_.isDefined).map(_.get)
  }

  def getNumsAroundCoord(coord: Coord, numbersMap: Map[Coord, IndexedInt]): Seq[Int] = {
    val foundNumbers = for {
      x <- coord.x - 1 to coord.x + 1
      y <- coord.y - 1 to coord.y + 1
    } yield numbersMap.get(Coord(x, y))

    val uniqueNumbers: Set[(Int, Int)] = foundNumbers.filter(_.isDefined).map(_.get).toSet
    uniqueNumbers.toSeq.map(_._1)
  }

  def part1(input: Schematic): Int = {
    val numbers: List[(Int, Bound)] = input.collect {
      case (Left(num), coord @ Coord(x, y)) => (num, Bound(Coord(x - digits(num) + 1, y), coord))
    }

    val coordsWithSymbols: Map[Coord, Char] = input.collect {
      case (Right(symbol), coord) => (coord, symbol)
    }.toMap

    numbers.filter {
      case (n, bound) => getSymbolsAroundBound(bound, coordsWithSymbols).nonEmpty
    }.map(_._1).sum
  }

  def part2(input: Schematic) = {
    val symbols: List[(Char, Coord)] = input.collect {
      case (Right(symbol), coord) => (symbol, coord)
    }

    // If the same number is in the grid multiple times, they will have different indexes
    // This way, we won't accidentally filter out gears which are adjacent to two of the same number
    val coordsWithNumbers: Map[Coord, IndexedInt] = input.collect {
      case (Left(num), coord) => (num, coord)
    }.zipWithIndex.map {
      case ((num, Coord(x, y)), index) => {
        val columns = x - digits(num) + 1 to x
        columns.map(col => (Coord(col, y), (num, index)))
      }
    }.flatten.toMap

    val gearNums = symbols.collect {
      case ('*', coord) => getNumsAroundCoord(coord, coordsWithNumbers)
    }.filter(_.size == 2)

    gearNums.map(_.product).sum
  }

  def main(args: Array[String]): Unit = {
    val input = parse("day03/input.txt")

    println("Day 03 part 1: " + part1(input))
    println("Day 03 part 2: " + part2(input))
  }
}
