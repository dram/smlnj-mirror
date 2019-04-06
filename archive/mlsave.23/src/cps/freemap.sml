signature FREEMAP =
  sig
    val freemap : ((CPS.lvar * CPS.lvar list) -> unit)
			-> (CPS.cexp -> CPS.lvar list)
    val freemapClose : CPS.cexp * (CPS.lvar -> bool)
			-> (CPS.lvar -> CPS.lvar list)
    val freemapSpill : (CPS.function * 'a) list * (CPS.lvar -> bool)
			-> (CPS.lvar -> CPS.lvar list)
  end

structure FreeMap : FREEMAP =
struct
open CPS SortedList

fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl nil = nil
  in  subl
  end

fun freemap add =
let fun setvars (w,free) = let val g = rem(w,free)
			   in add(w,g); g
			   end
    val rec freevars =
	 fn APP(v,args) => enter(v,uniq args)
	  | SWITCH(v,l) => hd(foldmerge ([v]::(map freevars l)))
	  | RECORD(l,w,ce) => merge(uniq (map #1 l), setvars(w, freevars ce))
	  | SELECT(_,v,w,ce) => enter(v, setvars(w, freevars ce))
	  | OFFSET(_,v,w,ce) => enter(v, setvars(w, freevars ce))
	  | PRIMOP(_,args,ret,ce) =>
	     let fun f(nil,a) = a
		   | f(w::wl,a) = f(wl,setvars(w,a))
	         val l =(case foldmerge(map freevars ce) of nil => nil | a::b => a)
	     in  merge(uniq args,f(ret,l))
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
    val vars : lvar list Intmap.intmap = Intmap.new Freemap
    val notconst = sublist (not o constant)
    fun setvars(v,l) = (Intmap.add vars (v,l); l)
    val rec freevars =
	 fn FIX(l,ce) =>
		let val functions = uniq(map #1 l)
		    val freel = map (fn(v,args,body) =>
				       setvars(v,remove(uniq args,freevars body)))
				    l
		in  remove(functions,hd(foldmerge((freevars ce)::freel)))
		end
	  | APP(v,args) => enter(v,notconst(uniq args))
	  | SWITCH(v,l) => hd(foldmerge ([v]::(map freevars l)))
	  | RECORD(l,w,ce) => merge(notconst(uniq(map #1 l)),
				    rem(w,freevars ce))
	  | SELECT(_,v,w,ce) => enter(v,rem(w,freevars ce))
	  | OFFSET(_,v,w,ce) => enter(v,rem(w,freevars ce))
	  | PRIMOP(_,args,ret,ce) =>
	      let val args = notconst(uniq args)
	      in  case foldmerge(map freevars ce)
	            of nil => args
		     | a::b => merge(args,remove(uniq ret,a))
	      end
in  freevars ce; Intmap.map vars
end

(* Produces a free variable mapping at each variable binding.
   It assumes FIXes have been eliminated.
   It assumes that the only lvars which refer to constants
   are the arguments of PRIMOPs, contents of RECORDs, and
   arguments of APPs.  In particular, the optimizer had better
   get rid of SWITCHes on constants. *)
fun freemapSpill(carg,constant) =
let exception FreemapSpill
    val vars : lvar list Intmap.intmap = Intmap.new FreemapSpill
    val notconst = sublist (not o constant)
    fun setvars(v,l) =
	let val l = rem(v,l)
	in  Intmap.add vars (v,l); l
	end
    val rec freevars =
	 fn FIX _ => ErrorMsg.impossible "FIX in cps/freemapSpill"
	  | APP(v,args) => enter(v,notconst(uniq args))
	  | RECORD(l,w,ce) => merge(notconst(uniq(map #1 l)),
				    setvars(w, freevars ce))
	  | SELECT(_,v,w,ce) => enter(v, setvars(w, freevars ce))
	  | OFFSET(_,v,w,ce) => enter(v, setvars(w, freevars ce))
	  | SWITCH(v,l) => hd(foldmerge([v]:: (map freevars l)))
	  | PRIMOP(_,args,ret as w::_,ce) =>
	      let val args = notconst(uniq args)
	      in  case foldmerge(map freevars ce)
	            of nil => (setvars(w,nil); args)
		     | a::b => merge(args, setvars(w,remove(uniq ret,a)))
	      end
	  | PRIMOP(_,args,nil,ce) => hd(foldmerge(notconst(uniq args)::(map freevars ce)))
in  app (fn ((_,_,b),_) => freevars b) carg; Intmap.map vars
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
val freemapSpill = timeit freemapSpill

end (* structure FreeMap *)

