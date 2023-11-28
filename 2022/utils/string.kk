module string

import utils/list

pub fun take(s : string, n : int) : string
  s.first(n).string

pub fun drop(s : string, n : int) : string
  val len = s.slice.count
  s.last(len - n).string

pub fun substrings-n(s : string, n : int) : div list<string>
  match s
    "" -> Nil
    _  -> Cons( s.take(n), substrings-n(s.drop(n), n) )
