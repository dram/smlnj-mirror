structure Sort=
struct

(* insertion sort. *)
fun sort (op > : ('x * 'x -> bool))
   = let fun insert (a, nil) = [a]
 	   | insert (a, l as hd::tl) = if a>hd then hd :: insert(a,tl) else a::l
	 fun s nil = nil
	   | s ((a: 'x)::rest) = insert (a, s rest)
      in s
     end

fun sorted (op >) =
  let fun s (x :: (rest as (y::_))) = y > x andalso s rest
        | s l = true
  in s
  end

end
