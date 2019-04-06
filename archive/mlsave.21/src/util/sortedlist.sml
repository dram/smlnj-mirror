structure SortedList =
struct

(* New uniq, hopefully more efficient because the comparison is inline. *)
(* Selection sort.  Order(n*n) time, creates no garbage, sorts integers
   into ascending order, eliminating duplicates. *)
fun uniq nil = nil
  | uniq (l as (hd::tl)) =
  let fun select(min:int, best, hd::tl) =
		select(min, if best>min
			    then if hd>min
				 then if best>hd
				      then hd
				      else best
				 else best
			    else hd, tl)
	| select(min, best, nil) = best
      fun lowest(best:int, hd::tl) = lowest( (if hd>best then best else hd), tl)
	| lowest(best, nil) = best;
      fun s min = let val v = select(min,hd,tl)
		  in  if v>min then v :: s v else nil
		  end
      val v = lowest(hd,tl)
  in  v :: s v
  end

(* old uniq
fun uniq l =
  let fun u nil = nil
	| u (l as [_]) = l
	| u (a::(tl as b::_)) = if a=b then u tl else a::u tl
  in  u (Sort.sort Integer.> l)
  end
*)
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
