module list

// https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.OldList.html#transpose
pub fun transpose(xss' : list<list<a>>) : <div,exn> list<list<a>>
  match xss'
    Nil -> Nil
    Cons(Nil, xss) -> xss.transpose
    Cons( Cons(x, xs), xss ) -> 
      fun combine(y : a, h : list<a>, ys : list<a>, t : list<list<a>>) : <div,exn> list<list<a>>
        Cons( Cons(y, h), Cons(ys, t).transpose )

      val (hds, tls) = xss.map fn(xs') { (xs'.head.unjust, xs'.tail) }.unzip
      combine(x, hds, xs, tls)

pub fun nub-by(xs' : list<a>, eq : (a, a) -> bool) : div list<a>
  match xs'
    Nil -> Nil
    Cons(x, xs) -> Cons( x, (xs.filter fn(y) { !eq(x, y) }).nub-by(eq) )
