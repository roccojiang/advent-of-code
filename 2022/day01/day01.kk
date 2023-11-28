import std/os/path
import std/os/file
import std/text/parse

import utils/input
import utils/sort

fun parse-elfs() : parse list<list<int>>
  many
    val elf = many
      val item = pint()
      pstring("\n")
      item
    pstring("\n")
    elf

// is there no way of not needing to define an anonymous function for sum?
fun elf-item-sums() : io list<int>
  "day01".get-input(parse-elfs).map(sum)


fun part1() : io int
  elf-item-sums().maximum

fun part2() : io int
  elf-item-sums().sort.reverse.take(3).sum
