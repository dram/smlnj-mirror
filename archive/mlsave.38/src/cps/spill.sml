signature SPILL =
  sig val spill : (CPS.function * bool) list * (CPS.lvar -> bool)
				 * (int * int * CPS.cexp -> CPS.cexp) ->
			(CPS.function * bool) list
  end

functor Spill(val maxfree : int) : SPILL =
struct
open FreeMap Access SortedList CPS
val error = ErrorMsg.impossible
fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl nil = nil
  in  subl
  end

local val spillname = Symbol.symbol "spillrec"
in    fun spillLvar() = namedLvar spillname
end

val ilist = PrintUtil.printClosedSequence ("[",",","]") Integer.print

fun last [x] = x
  | last (a::b) = last b

fun cut(0,_) = nil
  | cut(i,a::x) = a::cut(i-1,x)
  | cut(_,nil) = nil

fun nextuse x =
 let fun xin [] = false
       | xin((y,_)::r) = x=y orelse xin r
     fun xinl [] = false
       | xinl(y::r) = x=y orelse xinl r
     val rec f =
      fn ([],[],level) => level
       | ([],next,level) => f(next,[],level+1)
       | (SWITCH(v,l)::r,next,level) => if x=v then level else f(r,l@next,level)
       | (RECORD(l,w,c)::r,next,level) =>
	   if xin l then level else f(r,c::next,level)
       | (SELECT(i,v,w,c)::r,next,level) =>
	   if x=v then level else f(r,c::next,level)
       | (OFFSET(i,v,w,c)::r,next,level) =>
           if x=v then level else f(r,c::next,level)
       | (PRIMOP(i,a,w,cl)::r,next,level) =>
           if xinl a then level else f(r,cl@next,level)
       | (APP(v,vl)::r,next,level) =>
	   if x=v orelse xinl vl then level else f(r,next,level)
 in (fn y => f([y],[],0))
 end

fun show (SWITCH(v,l)) = (print "SWITCH "; print v; print "\n")
  | show (RECORD(_,w,_)) = (print "RECORD "; print w; print "\n")
  | show (SELECT(_,_,w,_)) = (print "SELECT "; print w; print "\n")
  | show (OFFSET(_,_,w,_)) = (print "OFFSET "; print w; print "\n")
  | show (PRIMOP(_,_,w::_,_)) = (print "PRIMOP "; print w; print "\n")
  | show (PRIMOP(_,vl,_,_)) = (print "PRIMOP "; ilist vl; print "\n")
  | show (APP(f,vl)) = (print "APP "; print f; ilist vl; print "\n")

local val sort = Sort.sort (fn ((i:int,_),(j,_)) => i>j)
in fun sortdups(cexp,dups) =
       map #2 (sort (map (fn dup as (v,w) => (nextuse v cexp, dup)) dups))
end

nonfix before
val \/ = merge and /\ = intersect
infix 6 \/   infix 7 /\
fun mash (constant,freevars) (formals,cexp) =
let fun f(results : lvar list,
	  uniques : lvar list,
          dups : (lvar*lvar) list,
 	  spill : (lvar list * lvar) option,
	  cexp : cexp) =
    let val prepare = (sublist (not o constant)) o uniq
        val (before,after) =  (* variables free before and after this
				 operation, 
	  			  not including the newly-bound variables *)
	 let val rec g =
	      fn SWITCH(v,l) => foldmerge(prepare[v] :: map g l)
	       | RECORD(l,w,c) =>  prepare (map #1 l) \/ freevars w
	       | SELECT(i,v,w,c) => prepare[v] \/freevars w
	       | OFFSET(i,v,w,c) => prepare[v] \/ freevars w
	       | PRIMOP(i,a,nil,cl) => foldmerge(prepare a :: map g cl)
	       | PRIMOP(i,a,res,cl) => prepare a \/ freevars(last res)
	       | APP(f,vl) => prepare(f::vl)
	     fun here(vl,wl) = (prepare vl \/ wl, wl)
	   in case cexp
	      of SWITCH(v,l) => here([v],foldmerge(map g l))
	       | RECORD(l,w,c) =>  here(map #1 l, freevars w)
	       | SELECT(i,v,w,c) => here([v],freevars w)
	       | OFFSET(i,v,w,c) => here([v],freevars w)
	       | PRIMOP(i,a,nil,cl) => here(a,foldmerge(map g cl))
	       | PRIMOP(i,a,res,cl) => here(a,freevars(last res))
	       | APP(f,vl) => here(f::vl,nil)
	 end

        val uniques = uniques \/ results
        val uniques_before = uniques /\ before
	val uniques_after = uniques /\ after
	val maxfree' = case spill of NONE => maxfree | SOME _ => maxfree-1
	val dups = cut(maxfree' - length(uniques_before \/ results),
		       sortdups(cexp,dups))

        val spill_after = 
	    case spill
	     of NONE => NONE
	      | s as SOME(contents,_) =>
		    case contents /\ after
		     of nil => NONE
		      | _ => s

        fun getpath v =
	  if constant v orelse member uniques_before v then (v, OFFp 0)
	  else let fun try((w,x)::l) = if v=w then (x,OFFp 0) else try l
		    | try nil = let val SOME (l,sv) = spill
			           fun find(i,w::l) = if v=w then (sv,SELp(i,OFFp 0))
						    else find(i+1,l)
				     | find(_,nil) = error "not found in spill"
			         in find(0,l)
			        end
	        in try dups
	       end

	fun makeSpillRec args =
	    let val contents = prepare args \/ after
	        val spillrec = map getpath contents
		val sv = spillLvar()
		val dups' = map (fn x => (x,x)) uniques_before @ dups
	     in (* ilist contents; print "\n";  *)
		RECORD(spillrec,sv,f(nil,nil,dups',SOME(contents,sv),cexp))
	    end

        fun g(args,res,conts,gen) = 
	  if length(prepare args \/ uniq res \/ uniques_after) > maxfree'
	    then makeSpillRec args
	    else let val paths = map (fn x => (x, getpath x)) (uniq args)
		     fun fetchit (_,(_,OFFp 0)) = false | fetchit _ = true
		  in case sublist fetchit paths
		      of [(v,(w,SELp(i,OFFp 0)))] =>
			    (* must fetch a variable from spill record *)
			    let val x = dupLvar v
		             in  (* print "Fetching "; print v; print "\n"; *)
				SELECT(i,w,x,f(nil,uniques_before,
					       (v,x)::dups,spill_after,cexp))
			    end
		      | (v,(w,SELp(i,OFFp 0)))::_ =>
			    (* must fetch more than one
				 variable from spill record *)
			    let val x = dupLvar v
		             in (* print "fetching "; print v; print "\n"; *)
			        SELECT(i,w,x,f(nil,uniques_before,
					     (v,x)::dups,spill,cexp))
			    end
		      | nil =>  (* no need for a fetch *)
			   let fun f' cexp = f(uniq res,uniques_after,
					       dups,spill_after,cexp)
			    in gen(map (#1 o getpath) args,res,map f' conts)
			   end
	         end

     in case ((* show cexp;*) cexp)
         of SWITCH(v,l) => g([v],[],l,fn([v],[],l)=>SWITCH(v,l))
          | RECORD(l,w,c) =>
           if 1+length(uniques_after) > maxfree'
	    then makeSpillRec(map #1 l)
	    else let val paths = map (fn(v,p) =>
				      let val (v',p') = getpath v
				       in (v', combinepaths(p',p))
				      end) l
		  in RECORD(paths,w,f([w],uniques_after,dups,spill,c))
		 end
          | SELECT(i,v,w,c) => g([v],[w],[c], fn([v],[w],[c])=>SELECT(i,v,w,c))
	  | OFFSET(i,v,w,c) => g([v],[w],[c], fn([v],[w],[c])=>OFFSET(i,v,w,c))
	  | PRIMOP(i,a,r,cl) => g(a,r,cl, fn(a,r,cl)=>PRIMOP(i,a,r,cl))
	  | APP(f,vl) => g(f::vl,[],[],fn(f::vl,[],[])=>APP(f,vl))
   end
 in f (nil,uniq formals, nil, NONE, cexp)
end

fun spill(carg,constant,prof) =
let val freevars = 
	let exception SpillFreemap
	    val m = Intmap.new(32, SpillFreemap) : lvar list Intmap.intmap
	    val _ = app (freemap constant (Intmap.add m) o #3 o #1) carg
         in Intmap.map m
        end
    exception TooMany
    fun checkv w = if length(freevars w) >= maxfree then raise TooMany else ()
    val rec check =
     fn FIX _ => error "FIX in cps/spill"
      | APP(l,args) => ()
      | SWITCH(v,l) => app check l
      | RECORD(l,w,c) => (checkv w; check c)
      | SELECT(i,v,w,c) => (checkv w; check c)
      | OFFSET(i,v,w,c) => (checkv w; check c)
      | PRIMOP(i,args,nil,l) => app check l
      | PRIMOP(i,args,w::_, l) => (checkv w; app check l)
 in  map (fn ((l,a,b),k) =>
	((l,a, ((check b; b) handle TooMany => mash (constant,freevars) (a,b))), k)) carg
end

end (* structure Spill *)
