(* dofree.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* This module inserts the "free" primop to dispose of each continuation
 record.  Continuation record creations are marked by the closure phase
 via the "record kind".
 Disposals are marked by the closure phase as follows: a variable bound
 by a SELECT or (more likely) as the formal parameter of a FIX will
 be marked with a cps-type (cty) of CNTt (continuation).  Such a marking
 on a bound variable means that at the time of binding, this variable
 is the only reference to the object AND no other variable will ever
 point to it; therefore when the last reference to the variable is passed,
 the record can be put back on the free list.

 This phase finds the last reference to each CNTt-type variable "v",
 and wraps SETTER(P.free,[v],...) around the following continuation-expression.

 You may ask, "why not just have the closure phase insert the P.free marking?"
 The answer is, the P.free marking is less reliable.  It violates some
 of the implicit rules of the CPS language.  Some future code generator
 may want to rearrange CPS expressions, and it would have to recompute
 the appropriate placement of the P.free marking anyway.

 So this DoFree implementation is a temporary stopgap anyway.

*)
  
structure DoFree : sig val dofree : CPS.function list -> CPS.function list 
		   end = 
struct
      open CPS

fun dofree (kind,f,args,cl,body) =
  let fun find (v::vr,CNTt::tr) = v
        | find (_::vr,_::tr) = find(vr,tr)
        | find _ = ~1
      fun newvar var =
       let fun z_is (VAR x) = x=var
	     | z_is _ = false
	   fun wrap (vl,f) el =
	       if exists #2 el orelse exists z_is vl
		   then let fun h(e,true) = e
		              | h(e,false) = SETTER(P.free,[var],e)
			 in (f(map h el), true)
                        end
                   else (f(map #1 el), false)
         val rec g =
         fn e as APP(v,args) => e
         | SWITCH(v,c,el) => 
	     wrap([v],fn el' => SWITCH(v,c,el')) (map g el)
         | SELECT(i,v,w,CNTt,e) => 
	      wrap([v],fn [e'] => SELECT(i,v,w,t,e')) [newvar w e]
         | SELECT(i,v,w,t,e) => 
	      wrap([v],fn [e'] => SELECT(i,v,w,t,e')) [g e]
         | RECORD(k,vl,w,e) => 
	      wrap(map #1 vl,fn [e'] => RECORD(k,vl,w,e')) [g e]
         | OFFSET(i,v,w,e) => 
	      wrap([v],fn [e'] => OFFSET(i,v,w,e')) [g e]
         | SETTER(p,vl,e) => 
	       wrap([vl],fn [e'] => SETTER(p,vl,e')) [g e]
         | LOOKER(p,vl,w,t,e) => 
	       wrap([vl],fn [e'] => LOOKER(p,vl,w,t,e')) [g e]
         | ARITH(p,vl,w,t,e) => 
	       wrap([vl],fn [e'] => ARITH(p,vl,w,t,e')) [g e]
         | PURE(p,vl,w,t,e) => 
	       wrap([vl],fn [e'] => PURE(p,vl,w,t,e')) [g e]
         | BRANCH(p,vl,c,e1,e2) => 
	       wrap([vl],fn [e1',e2'] => BRANCH(p,vl,c,e1',e2')) [g e1, g e2]
         | FIX _ => error "FIX in Freemap.freemap"
       in  g
       end
  in  newvar (find(args,cl)) body
  end

end

(*
 * $Log: dofree.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:44  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:31  george
 *   Version 109.24
 *
 *)
