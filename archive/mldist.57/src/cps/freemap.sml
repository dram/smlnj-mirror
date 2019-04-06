signature FREEMAP =
  sig
    val freemap : (CPS.lvar -> bool)
		    -> (CPS.lvar * CPS.lvar list -> unit)
			-> (CPS.cexp -> CPS.lvar list)
    val freemapClose : CPS.cexp * (CPS.lvar -> bool)
			-> ((CPS.lvar -> (CPS.lvar * CPS.lvar list)) *
			    (CPS.lvar -> bool) *
			    (CPS.lvar -> bool) *
			    (CPS.lvar -> bool))
  end

structure FreeMap : FREEMAP =
struct
open CPS SortedList

fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl nil = nil
  in  subl
  end

fun freemap constant add =
let val clean = (sublist (not o constant)) o uniq
    val enter = fn (x,y) => if constant x then y else enter(x,y)
    fun setvars (w,free) = let val g = rmv(w,free)
			   in add(w,g); g
			   end
    val rec freevars =
	 fn APP(v,args) => enter(v,clean args)
	  | SWITCH(v,l) => enter(v,foldmerge (map freevars l))
	  | RECORD(l,w,ce) => merge(clean (map #1 l), setvars(w, freevars ce))
	  | SELECT(_,v,w,ce) => enter(v, setvars(w, freevars ce))
	  | OFFSET(_,v,w,ce) => enter(v, setvars(w, freevars ce))
	  | PRIMOP(_,args,ret,ce) =>
	     let fun f(nil,a) = a
		   | f(w::wl,a) = f(wl,setvars(w,a))
	     in  merge(clean args,f(ret,foldmerge(map freevars ce)))
	     end
	  | FIX _ => ErrorMsg.impossible "FIX in Freemap.freemap"
in freevars
end

(* Produces a free variable mapping at each function binding.
   The mapping includes the functions bound at the FIX, but
   not the arguments of the function.
   It assumes that the only lvars which refer to constants
   are the arguments of PRIMOPs, contents of RECORDs, and
   arguments of APPs.  In particular, the optimizer had better
   get rid of SWITCHes on constants. *)
fun freemapClose(ce,constant) =
let exception Freemap
    val vars : (lvar * lvar list) Intmap.intmap = Intmap.new(32, Freemap)
    val notconst = sublist (not o constant)
    val escapes = Intset.new()
    val escapesP = Intset.mem escapes
    val escapesM = Intset.add escapes
    val known = Intset.new()
    val knownM = Intset.add known
    val unknown = Intset.new()
    val unknownM = Intset.add unknown
    val rec freevars =
	 fn FIX(l,ce) =>
		let val functions = uniq(map #1 l)
		    (* MUST be done in this order due to side-effects *)
		    val freeb = freevars ce
		    val (labs,freel) =
			fold (fn ((v,args,body),(labs,freel)) =>
			       (let val label = Access.dupLvar v
				    val l = remove(uniq args,freevars body)
				in  Intmap.add vars (v,(label,l));
				    ((v,label)::labs,l::freel)
				end))
			      l (nil,nil)
	            fun m(v,label) = if escapesP v then unknownM label
				     else knownM label
		in  map m labs;
		    remove(functions,foldmerge(freeb::freel))
		end
	  | APP(v,args) => (app escapesM args;
			    enter(v,notconst(uniq args)))
	  | SWITCH(v,l) => foldmerge ([v]::(map freevars l))
	  | RECORD(l,w,ce) => (app (escapesM o #1) l;
			       merge(notconst(uniq(map #1 l)),
				    rmv(w,freevars ce)))
	  | SELECT(_,v,w,ce) => enter(v,rmv(w,freevars ce))
	  | OFFSET(_,v,w,ce) => enter(v,rmv(w,freevars ce))
	  | PRIMOP(_,args,ret,ce) =>
	      let val args = notconst(uniq args)
	      in  app escapesM args;
		  merge(args,remove(uniq ret,foldmerge(map freevars ce)))
	      end
in  freevars ce;
    (Intmap.map vars, Intset.mem escapes, Intset.mem unknown, Intset.mem known)
end

(* temporary, for debugging *)
fun timeit f a =
  let val t = System.Timer.start_timer()
      val r = f a
  in  System.Stats.update(System.Stats.freemap,System.Timer.check_timer t);
      r
  end
val freemap = timeit freemap
val freemapClose = timeit freemapClose

end (* structure FreeMap *)

