structure Sort=
struct

(* insertion sort.  probably correct (not tested), but creates
   O(n*n) garbage 
fun sort (op > : ('x * 'x -> bool))
   = let fun insert (a, nil) = [a]
 	   | insert (a, hd::tl) = if a>hd then hd :: insert(a,tl);
	 fun s nil = nil
	   | s (a: 'x)::rest = insert (a, s rest)
      in s
     end
*)

(* selection sort.  Ugly, but is correct and tested, and creates no
   garbage *)

fun sort (op > : ('x * 'x -> bool))
   = let fun select(min, best, hd::tl) = select(min,
					  if best > min
					   then if best > hd andalso hd > min
						 then hd else best
					   else hd,
					  tl)
	   | select(min, best, nil) = best;
	 fun lowest(best, hd::tl) = lowest( (if hd>best then best else hd), tl)
	   | lowest(best, nil) = best;
         fun s (l as (hd::tl), min) = let val v = select(min,hd,tl)
				in if v > min then v :: s(l, v)
					      else nil
				end
	   | s _ = (print "impossible in sort"; case 9 of 8 => nil)
      in fn (l as (hd::tl)) => let val v = lowest(hd,tl) in v :: s(l, v) end
	  | nil => nil
     end;

fun sorted (op>) =
  let fun s nil = true
        | s [x] = true
	| s (x :: (rest as (y::_))) = y>x andalso s rest
   in s
  end

end
