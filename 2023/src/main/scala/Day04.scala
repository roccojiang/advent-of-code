package day04

import scala.io.Source

object Day04 {
  type Card = (Seq[Int], Seq[Int])

  def parse(filepath: String): Seq[Card] = {
    val input = Source.fromResource(filepath).getLines().toSeq
    input.map(parseCard)
  }

  def parseCard(card: String): Card = {
    val Array(winning, nums) = card.split(':').last.split('|').map(_.split(' ').filterNot(_ == "").map(_.toInt).toSeq)
    (winning, nums)
  }

  def calcCardWins(card: Card): Int = {
    val (winning, nums) = card
    nums.filter(winning.contains(_)).size
  }

  def part1(cards: Seq[Card]): Int = {
    def calcCardPoints(card: Card): Int = {
      val wins = calcCardWins(card)
      if (wins == 0) 0 else 1 << wins - 1 // 2 ^ (number of wins - 1)
    }

    cards.map(calcCardPoints).sum
  }

  def part2(cards: Seq[Card]): Int = {
    val copies = Array.fill(cards.size)(1)

    cards.map(calcCardWins).zipWithIndex.foreach {
      case (wins, i) => (i + 1 to i + wins).foreach(copies(_) += copies(i))
    }

    copies.sum
  }

  def main(args: Array[String]): Unit = {
    val input = parse("day04/input.txt")

    println("Day 04 part 1: " + part1(input))
    println("Day 04 part 2: " + part2(input))
  }
}
