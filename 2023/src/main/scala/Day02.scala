package day02

import parsley.Parsley
import parsley.character._
import parsley.combinator._
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.token.predicate

import scala.io.Source

sealed trait Cube
case object Red extends Cube
case object Green extends Cube
case object Blue extends Cube

// Absolute overkill, but I wanted to play with Parsley
object lexer {
  val lexer = new Lexer(LexicalDesc.plain.copy(
    spaceDesc = SpaceDesc.plain.copy(
      space = predicate.Basic(_.isSpaceChar)
    )
  ))

  val implicits = lexer.lexeme.symbol.implicits
  val num: Parsley[Int] = lexer.lexeme.numeric.unsigned.decimal.map(_.toInt)
}

object Day02 {
  import lexer.implicits.implicitSymbol
  import lexer.num

  type Round = Map[Cube, Int]
  type Game = List[Round]

  object parser {
    def toRoundMap(cubesDrawn: List[(Cube, Int)]): Round = {
      val emptyMap: Map[Cube, Int] = Map(Red -> 0, Green -> 0, Blue -> 0)
      emptyMap ++ cubesDrawn.toMap
    }

    val cubeDrawn = (num <~> atomicChoice("red" #> Red, "green" #> Green, "blue" #> Blue)).map(_.swap)
    val round = sepBy1(cubeDrawn, ",").map(toRoundMap)
    val rounds = sepBy1(round, ";")
    val game = "Game" ~> num ~> ":" ~> rounds
    val games = sepBy1(game, newline)
  }

  def minPossibleCubes(rounds: List[Round]): Round = 
    rounds.reduceLeft { (acc, round) =>
      acc ++ round.map { case (cube, amount) =>
        cube -> Math.max(acc(cube), amount)
    }}

  def parse(filepath: String): List[Game] = {
    val input = Source.fromResource(filepath).getLines().toSeq.mkString("\n")
    parser.games.parse(input).get
  }

  def part1(games: List[Game]): Int = {
    val minPossibleCubesPerGame = games.map(minPossibleCubes)
    val possibleGames = minPossibleCubesPerGame.zipWithIndex.filter {
      case (cubes, _) => cubes(Red) <= 12 && cubes(Green) <= 13 && cubes(Blue) <= 14
    }
    possibleGames.map(_._2 + 1).sum
  }

  def part2(games: List[Game]): Int = {
    val minPossibleCubesPerGame = games.map(minPossibleCubes)
    minPossibleCubesPerGame.map(_.values.product).sum
  }

  def main(args: Array[String]): Unit = {
    val games = parse("day02/input.txt")

    println("Day 02 part 1: " + part1(games))
    println("Day 02 part 2: " + part2(games))
  }
}
