package day03

import parsley.Parsley
import parsley.character._
import parsley.combinator._
import parsley.position._
import parsley.implicits.character.stringLift

import scala.io.Source

object Day03 {
  type Coord = (Int, Int)
  type Bound = (Coord, Coord)

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
    val row = (many(".") ~> many(numOrSymbol <~> pos.map(p => (p._1 - 1, p._2 - 1 - 1)) <~ many(".")))

    val schematic = sepBy(row, newline)
  }

  def digits(n:Int) = if (n==0) 1 else math.log10(math.abs(n)).toInt + 1;

  def parse(filepath: String): Schematic = {
    val input = Source.fromResource(filepath).getLines().toSeq.mkString("\n")
    parser.schematic.parse(input).get.flatten
  }

  def getSymbolsAroundBound(bounds: Bound, symbolsMap: Map[Coord, Char]): Seq[Char] = {
    val foundSymbols = for {
      row <- bounds._1._1 - 1 to bounds._2._1 + 1
      col <- bounds._1._2 - 1 to bounds._2._2 + 1
    } yield symbolsMap.get((row, col))

    foundSymbols.filter(_.isDefined).map(_.get)
  }

  def getNumsAroundCoord(coord: Coord, numbersMap: Map[Coord, (Int, Int)]): Seq[Int] = {
    val foundNumbers = for {
      row <- coord._1 - 1 to coord._1 + 1
      col <- coord._2 - 1 to coord._2 + 1
    } yield numbersMap.get((row, col))

    val uniqueNumbers: Set[(Int, Int)] = foundNumbers.filter(_.isDefined).map(_.get).toSet
    uniqueNumbers.toSeq.map(_._1)
  }

  def part1(input: Schematic): Int = {
    val numbers: List[(Int, Bound)] = input.collect {
      case (Left(num), (row, col)) => (num, ((row, col - digits(num) + 1), (row, col)))
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

    // Ugly hack to ensure we can filter out multiple matches to the same actual number
    // If the same number is in the grid multiple times, they will have different ids
    // This way, we won't accidentally filter out gears which are adjacent to two of the same number
    var id = 0
    val coordsWithNumbers: Map[Coord, (Int, Int)] = input.collect {
      case (Left(num), (row, col)) => {
        val columns = col - digits(num) + 1 to col
        id += 1
        columns.map(col => ((row, col), (num, id)))
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
