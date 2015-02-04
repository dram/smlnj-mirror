(* conrep.sml *)

structure ConRep = struct

local open Basics BasicTypes in

fun count predicate
  = let fun test (a::rest) = if predicate a then 1 + test rest else test rest
	  | test nil = 0
     in test
    end

fun boxed [DATACON{rep,const=false,...}] = rep := TRANSPARENT
  | boxed dcons =
	let val n = count (fn (DATACON{const,...}) => not const) dcons
	    fun decide (i,j,((DATACON{rep,const=true,...})::rest)) =
		    (rep := CONSTANT i; decide(i+1,j,rest))
	      | decide (i,j,((DATACON{rep,vtype=CONty(_,[ty,_]),...})::rest)) =
		    (case (n,ty)
		      of (1,CONty(ref(RECORDtyc{labels= _::_::_, ...}),_)) =>
			    (rep := TRANSPARENT; decide(i,j,rest))
		      | _ => (rep := TAGGED j; decide(i,j+1,rest)))
	      | decide (_,_,nil) = ()
	 in decide(0,0,dcons)
	end

end (* local *)

end (* structure ConRep *)
