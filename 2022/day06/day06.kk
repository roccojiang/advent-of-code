import std/os/file
import std/os/path

import utils/list

fun get-input() : io string
  read-text-file(cwd()/"day06/input.txt").trim

fun unique-chars(s : string) : div bool
  val cs = s.list
  cs.string == cs.nub-by fn(a, b) { a == b }.string

fun look-for-marker(buffer : string, n : int) : <exn,div> int
  fun go(remaining : list<char>, pos : int) : <exn,div> int
      val candidate = remaining.take(n)
      if candidate.length < n               then throw("marker never found")
      else if candidate.string.unique-chars then pos
      else                                  go(remaining.drop(1), pos + 1)
  
  go(buffer.list, n)

fun part1() : io int
  get-input().look-for-marker(4)

fun part2() : io int
  get-input().look-for-marker(14)
