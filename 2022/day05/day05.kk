import std/os/path
import std/os/file
import std/text/parse

import utils/input
import utils/list
import utils/string

alias crate = char
alias stack = list<crate>
fun show(s : stack) : string
  "stack: " ++ s.string

alias move = (int, int, int)
fun show(m : move) : string
  val (cnt, from, to) = m
  "move " ++ cnt.show ++ " from " ++ from.show ++ " to " ++ to.show

// state of list of a
effect state<a>
  fun get(i : int) : a
  fun put(x : a, i : int) : ()

fun run-state(init : list<a>, action : () -> <state<a>,exn,div|e> b) : <exn,div|e> (b, list<a>)
  var st := init.vector
  with handler
    return(x)     (x, st.list)
    fun get(i)    st[i]
    fun put(x, i) st[i] := x 
  action()

fun push(i : int, c : crate) : state<stack> ()
  val stack = get(i)
  Cons(c, stack).put(i)

fun pop(i : int) : <state<stack>,exn> crate
  val stack : list<crate> = get(i)
  stack.tail.put(i)
  stack.head.unjust

fun pushs(i : int, cs : list<crate>) : state<stack> ()
  val stack = get(i)
  (cs ++ stack).put(i)

fun pops(i : int, n : int) : <state<stack>,exn> list<crate>
  val stack : list<crate> = get(i)
  stack.drop(n).put(i)
  stack.take(n)


fun parse-crate() : parse crate
  char('[')
  val crate = alpha()
  char(']')
  crate

fun parse-stack() : parse stack
  whitespace()
  pint()
  whitespace()
  val crates = many
    val crate = parse-crate()
    whitespace0()
    crate

  crates.reverse

fun parse-move() : parse move
  pstring("move")
  whitespace()
  val cnt = pint()
  whitespace()
  pstring("from")
  whitespace()
  val from = pint()
  whitespace()
  pstring("to")
  whitespace()
  val to = pint()

  (cnt, from, to)


fun get-parsed-input() : io (list<stack>, list<move>) 
  val (raw-stacks, raw-instrs) = read-text-file(cwd()/"day05/input.txt").lines.span fn(s) { s != "" }
  val unparsed-stacks = raw-stacks.map fn(s) { s.substrings-n(4) }.transpose.map fn(s) { s.reverse.join }
  val unparsed-instrs = raw-instrs.filter fn(s) { !s.list.all(is-white) }

  val stacks = unparsed-stacks.map fn(s) { s.slice.parse(parse-stack).get-parsed }
  val instrs = unparsed-instrs.map fn(i) { i.slice.parse(parse-move).get-parsed }

  (stacks, instrs)


fun top-crate(stack : stack) : exn crate
  stack.head.unjust

fun exec-move-pt1(move : move) : <state<stack>,exn> ()
  val (cnt, from, to) = move
  repeat(cnt)
    val crate = pop(from - 1)
    push(to - 1, crate)

fun exec-move-pt2(move : move) : <state<stack>,exn> ()
  val (cnt, from, to) = move
  val crates = pops(from - 1, cnt)
  pushs(to - 1, crates)


fun concat-top-crates(exec-move : move -> <state<stack>,exn> ()) : io string
  val (stacks, instrs) = get-parsed-input()
  val (_, state) = run-state(stacks){instrs.foreach(exec-move)}
  state.map(top-crate).string

fun part1() : io string
  concat-top-crates(exec-move-pt1)

fun part2() : io string
  concat-top-crates(exec-move-pt2)
