import std/os/path
import std/os/file
import std/text/parse

import utils/input

alias rucksack = list<char>

fun parse-rucksack() : parse rucksack
  val rucksack = many
    alpha()
  pstring("\n")
  rucksack

fun parse-individual-rucksacks() : parse list<rucksack>
  many(parse-rucksack)

fun parse-grouped-rucksacks() : parse list<list<rucksack>>
  many
    count(3, parse-rucksack)


fun compartments(rucksack : list<char>) : (list<char>, list<char>)
  val i = rucksack.length / 2
  rucksack.split(i)

fun priority(c : char) : exn int
  if c.is-lower      then c.int - 'a'.int + 1
  else if c.is-upper then c.int - ('A'.int - 1) + ('Z'.int - 'A'.int + 1)
  else                    throw("priority: non-alphabetical character")

fun common-items(xs : list<char>, ys : list<char>) : list<char>
  // how to have polymorphic (==) so that elem can also be polymorphic?
  fun elem(c : char, cs : list<char>) : bool
    match cs.find fn(c') { c' == c }
      Just(_) -> True
      Nothing -> False

  xs.filter fn(x) { x.elem(ys) }

fun common-item(xs : list<char>, ys : list<char>) : exn char
  // Pre: only one item is in common
  val commons = common-items(xs, ys)
  val first = match commons.head
    Just(item) -> item
    Nothing    -> throw("common-item: no common item found between " ++ xs.string ++ " and " ++ ys.string)

  if commons.all fn(item) { item == first } then first
  else throw("common-item: more than one item found to be in common: " ++ commons.string)


fun part1() : io int
  "day03".get-input(parse-individual-rucksacks)
  .map fn(rucksack)
    val (c1, c2) = rucksack.compartments
    common-item(c1, c2).priority
  .sum

fun part2() : io int
  "day03".get-input(parse-grouped-rucksacks)
  .map fn(trio) 
    // Pre: trio list contains 3 rucksacks
    val Cons(elf1, Cons(elf2, Cons(elf3, Nil))) = trio
    common-items(elf1, elf2).common-item(elf3)
    .priority
  .sum
