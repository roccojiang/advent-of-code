package day02

import scala.io.Source
import scala.util.Using

object Day02 {

  private val day02input = Using.resource(Source.fromResource("day02.txt"))(
    _.getLines()
      .map(_.split(" ") match { case Array(cmd, i) => (cmd, i.toInt) })
      .toSeq
  )

  def main(args: Array[String]) = {
    println("Part 1: " + part1(day02input))
    println("Part 2: " + part2(day02input))
  }

  case class Submarine(val pos: Int, val depth: Int, val aim: Int)

  def part1(commands: Seq[(String, Int)]): Int = {
    val sub = commands.foldLeft(Submarine(0, 0, 0)) {
      case (Submarine(pos, depth, _), (cmd, x)) => {
        cmd match {
          case "forward" => Submarine(pos + x, depth, 0)
          case "down"    => Submarine(pos, depth + x, 0)
          case "up"      => Submarine(pos, depth - x, 0)
        }
      }
    }

    sub.pos * sub.depth
  }

  def part2(commands: Seq[(String, Int)]): Int = {
    val sub = commands.foldLeft(Submarine(0, 0, 0)) {
      case (Submarine(pos, depth, aim), (cmd, x)) =>
        cmd match {
          case "forward" => Submarine(pos + x, depth + x * aim, aim)
          case "down"    => Submarine(pos, depth, aim + x)
          case "up"      => Submarine(pos, depth, aim - x)
        }
    }

    sub.pos * sub.depth
  }
}
