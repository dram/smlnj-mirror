structure SortedList =
struct

fun uniq l =
  let fun u nil = nil
	| u (l as [_]) = l
	| u (a::(tl as b::_)) = if a=b then u tl else a::u tl
  in  u (Sort.sort Integer.> l)
  end
fun merge(a,nil) = a
  | merge(nil,a) = a
  | merge(l as (i:int)::a, m as j::b) = 
	if i<j then i::merge(a,m)
	else if i>j then j::merge(l,b)
	else merge(l,b)
fun remove(x as (xl:int)::xr, y as yl::yr) = if xl>yl then yl::remove(x,yr)
					else if xl<yl then remove(xr,y)
					else remove(xr,yr)
  | remove(nil, y) = y
  | remove _ = nil
fun enter(new:int,l) =
  let fun f nil = [new]
	| f (l as h::t) = if new=h then l else if new < h then new::l else h::f t
  in  f l
  end
fun member l (e:int) =
  let fun f nil = false
	| f (h::t) = h=e orelse h<e andalso f t
  in  f l
  end
fun intersect(nil,_) = nil
  | intersect(_,nil) = nil
  | intersect(l as (a:int)::b,r as c::d) =
	if a=c then a::intersect(b,d)
	else if a<c then intersect(b,r)
	else intersect(l,d)
fun difference(nil,_) = nil
  | difference(l,nil) = l
  | difference(l as (a:int)::b,r as c::d) =
	if a=c then difference(b,d)
	else if a<c then a::difference(b,r)
	else difference(l,d)	
end
