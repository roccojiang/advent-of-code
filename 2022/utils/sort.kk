module sort

fun insert(e : int, sorted : list<int>) : list<int>
  match sorted
    Nil         -> Cons(e, Nil)
    Cons(x, xs) -> if e < x then Cons(e, Cons(x, xs)) else Cons(x, e.insert(xs))

pub fun sort(es: list<int>) : list<int>
  match es
    Nil         -> es
    Cons(x, xs) -> insert(x, xs.sort)
