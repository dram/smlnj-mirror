(* Copyright 1989 by AT&T Bell Laboratories *)
structure Sort = struct

(* Industrial-strength quicksort.
   Selects pivot from middle of input list.
   Distributes elements equal to pivot "randomly" in the two output partitions.
   Special-cases lists of 0, 1, or 2 elements.
*)
(* This sort function breaks the compiler!
 * symptoms: cannot load mlyacc. - lg
 *)
fun sort (op > : ('x * 'x -> bool)) =
  let fun splita(pivot,nil,less,greater)= qsort less @ (pivot :: qsort greater)
        | splita(pivot,a::rest,less,greater) =
	             if a>pivot then splitb(pivot,rest,less,a::greater)
			        else splitb(pivot,rest,a::less,greater)
      and splitb(pivot,nil,less,greater)= qsort less @ (pivot :: qsort greater)
        | splitb(pivot,a::rest,less,greater) =
	             if pivot>a then splita(pivot,rest,a::less,greater)
			        else splita(pivot,rest,less,a::greater)
      and split1a(pivot,0,_::r,less,greater) = splitb(pivot,r,less,greater)
        | split1a(pivot,i,a::rest,less,greater) =
	             if a>pivot then split1b(pivot,i-1,rest,less,a::greater)
			        else split1b(pivot,i-1,rest,a::less,greater)
      and split1b(pivot,0,_::r,less,greater) = splita(pivot,r,less,greater)
        | split1b(pivot,i,a::rest,less,greater) =
	             if pivot>a then split1a(pivot,i-1,rest,a::less,greater)
			        else split1a(pivot,i-1,rest,less,a::greater)
      and qsort (l as [a,b]) = if a>b then [b,a] else l
        | qsort (l as _::_::_) = 
           let fun getpivot (x::xr, _::_::rest, i) = getpivot(xr,rest,i+1)
                 | getpivot (x::_, _,i) = split1a(x,i,l,nil,nil)
            in getpivot(l,l,0)
           end
        | qsort l = l
   in qsort
  end

fun sort (op > : ('x * 'x -> bool)) =
     let fun s (a::b::c) =
 	    let val (x,y) = if a>b then (b,a) else (a,b)
 		fun insert' [] = [y]
 		  | insert' (l as c::d) = if y>c then c::insert' d else y::l
 		fun insert [] = [x,y]
  	          | insert (l as c::d) = 
		    if x>c then c::insert d else x::insert' l
 	    in insert(s c)
 	    end
 	  | s l = l
     in s
     end


fun sorted (op >) =
  let fun s (x::(rest as (y::_))) = not(x>y) andalso s rest
        | s l = true
  in s
  end

end
