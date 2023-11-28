package day03

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03 {

  private val day03input = Using.resource(Source.fromResource("day03.txt"))(
    _.getLines()
     .map(_.split("")
           .map(_.toInt)
           .toSeq)
     .toSeq
  )

  type Bit = Int
  type Binary = Seq[Bit]

  def main(args: Array[String]) = {
    println("Part 1: " + part1(day03input))
    println("Part 2: " + part2(day03input))
  }

  def part1(binaries: Seq[Binary]) = {
    val transposed = binaries.transpose

    val gamma = binaryToInt(transposed.map(mostCommonBit))
    val epsilon = binaryToInt(transposed.map(leastCommonBit))

    gamma * epsilon
  }

  def part2(binaries: Seq[Binary]): Int = {
    val oxygen = binaryToInt(calcRating(binaries, mostCommonBit))
    val co2 = binaryToInt(calcRating(binaries, leastCommonBit))

    oxygen * co2
  }

  private def binaryToInt(binary: Binary): Int =
    Integer.parseInt(binary.mkString, 2)

  private def mostCommonBit(bits: Binary): Bit =
    if (2 * bits.count(_ == 1) >= bits.size) 1 else 0

  private def leastCommonBit(bits: Binary): Bit =
    1 - mostCommonBit(bits)

  private def calcRating(binaries: Seq[Binary], f: Binary => Bit): Binary = {
    @tailrec
    def calcRating0(binaries: Seq[Binary], i: Int, f: Binary => Bit): Binary = {
      val transposed = binaries.transpose
      val bit = f(transposed(i))

      val newBinaries = binaries.filter(_(i) == bit)

      newBinaries match {
        case Seq(x) => x
        case _      => calcRating0(newBinaries, i + 1, f)
      }
    }

    calcRating0(binaries, 0, f)
  }
}
