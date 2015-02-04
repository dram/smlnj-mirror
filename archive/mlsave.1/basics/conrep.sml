(* conrep.sml *)

structure ConRep = struct

local open Basics BasicTypes in

fun count predicate
  = let fun test (a::rest) = if predicate a then 1 + test rest else test rest
	  | test nil = 0
     in test
    end

fun boxed (DATAtyc{dcons=ref dcons,...}) =
	let val n = count (fn (DATACON{const,...}) => not const) dcons
	    val len = length(dcons)
	    fun unboxed (d as DATACON{rep,vtype,const,...}) =
		(case !rep
		 of TAGGED _ => false
		  | CONSTANT _ => true
		  | TRANSPARENT => let val CONty(_,[ty,_]) = vtype
				    in not(boxedty ty)
				   end
		  | TRANSB => false
		  | TRANSU => true
		  | REF => false
		  | VARIABLE _ => false
		  | UNDECIDED => true)
	    fun decide (i,j,((d as DATACON{rep,vtype,const,...})::rest)) =
		    (if const then (rep := CONSTANT i; decide(i+1,j,rest))
		     else if n= 1 andalso
			  (len = 1
		              orelse (let val CONty(_,[ty,_]) = vtype
				       in rep := CONSTANT 0;
					  boxedty ty
				      end))
			    then (rep := TRANSPARENT; decide(i,j,rest))
			    else (rep := TAGGED j; decide(i,j+1,rest));
		     (* PrintBasics.printCon d;*) ())
	      | decide (_,_,nil) = ()
	 in (case dcons
	      of DATACON{rep=ref UNDECIDED,...}::_ => decide(0,0,dcons)
	       | _ => ());
	    not(exists(unboxed,dcons))
	end
  | boxed (RECORDtyc {labels=nil,...}) = false
  | boxed (RECORDtyc _) = true
  | boxed (TYPEtyc{def,...}) = boxedty def
  | boxed (tyc as ATOMtyc _) =
		eqTycon(tyc, !arrowTycon)
	orelse  eqTycon(tyc, !realTycon)
	orelse  eqTycon(tyc, !exnTycon)
  | boxed _ = false

and boxedty (CONty(ref tyc,_)) = boxed tyc
  | boxedty (VARty(TYVAR{status=ref(INSTANTIATED ty),...})) = boxedty ty
  | boxedty _ = false

end (* local *)

end (* structure ConRep *)
