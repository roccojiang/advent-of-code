import std/os/path
import std/os/file
import std/text/parse

import utils/input

alias section = (int, int)

fun parse-section() : parse section
  val low = pnat()
  char('-')
  val high = pnat()
  (low, high)

fun parse-section-pairs() : parse list<(section, section)>
  many
    val s1 = parse-section()
    char(',')
    val s2 = parse-section()
    char('\n')
    (s1, s2)


fun fully-contains(pair : (section, section)) : bool
  val ((a, b), (c, d)) = pair
  a <= c && b >= d || a >= c && b <= d

fun overlaps(pair : (section, section)) : bool
  val ((a, b), (c, d)) = pair
  a <= d && b >= c


effect fun cond(pair : (section, section)) : bool

fun count-pairs() : <io,cond> int
  "day04".get-input(parse-section-pairs).filter(cond).length

fun part1() : io int
  with fun cond(p) p.fully-contains
  count-pairs()

fun part2() : io int
  with fun cond(p) p.overlaps
  count-pairs()
