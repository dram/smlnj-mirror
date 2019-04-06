(* Copyright 1989 by AT&T Bell Laboratories *)
signature SPILL =
  sig val spill : (CPS.function * bool) list
				 * (int * int * CPS.cexp -> CPS.cexp) ->
			(CPS.function * bool) list
  end

functor Spill(val maxfree : int) : SPILL =
struct

structure SortList =
(* this structure is duplicated here for efficiency *)
struct

fun enter(new:int,l) =
  let fun f [] = [new]
	| f (l as h::t) = if new<h then new::l else if new>h then h::f t else l
  in  f l
  end

fun uniq l =
    let fun loop([],acc) = acc
	  | loop(a::r,acc) = loop(r,enter(a,acc))
    in loop(l,[])
    end

fun merge(a,[]) = a
  | merge([],a) = a
  | merge(l as (i:int)::a, m as j::b) = 
      if j<i then j::merge(l,b) else i::merge(a,if i<j then m else b)

local fun loop (a::b::rest) = loop(merge(a,b)::loop rest)
        | loop l = l
in fun foldmerge l = hd(loop l) handle Hd => []
end

fun remove(x as (xl:int)::xr, y as yl::yr) =
    if xl>yl then yl::remove(x,yr) else remove(xr,if xl<yl then y else yr)
  | remove(_,y) = y

fun rmv (x : int,l) =
    let fun loop nil = nil
	  | loop (a::b) = if x=a then b else a::loop b
    in loop l
    end

fun member l (e:int) =
  let fun f [] = false
	| f (h::t) = if h<e then f t else e=h
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


open FreeMap Access SortList CPS
val error = ErrorMsg.impossible
fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl [] = []
  in  subl
  end

local val spillname = Symbol.symbol "spillrec"
in    fun spillLvar() = namedLvar spillname
end

val ilist = PrtUtil.printClosedSequence ("[",",","]") Integer.print

val pr = outputc std_out
fun sayv(VAR v) = pr(Access.lvarName v)
  | sayv(LABEL v) = pr("(L)" ^ Access.lvarName v)
  | sayv(INT i) = pr(makestring i)
  | sayv(REAL r) = pr r
  | sayv(STRING s) = (pr "\""; pr s; pr "\"")
val vallist = PrtUtil.printClosedSequence("[",",","]") sayv

fun cut(0,_) = []
  | cut(i,a::x) = a::cut(i-1,x)
  | cut(_,[]) = []

fun nextuse x =
 let fun xin[] = false | xin(VAR y::r) = x=y orelse xin r | xin(_::r) = xin r
     fun g(level,a) =
     let val rec f =
      fn ([],[]) => level
       | ([],next) => g(level+1,next)
       | (SWITCH(v,l)::r,next) => if xin[v] then level else f(r,l@next)
       | (RECORD(l,w,c)::r,next) =>
	 if xin(map #1 l) then level else f(r,c::next)
       | (SELECT(i,v,w,c)::r,next) => if xin[v] then level else f(r,c::next)
       | (OFFSET(i,v,w,c)::r,next) => if xin[v] then level else f(r,c::next)
       | (PRIMOP(i,a,w,cl)::r,next) => if xin a then level else f(r,cl@next)
       | (APP(v,vl)::r,next) => if xin(v::vl) then level else f(r,next)
     in f(a,[])
     end
     fun h y = g(0,[y])
 in h
 end

local val sort = Sort.sort (fn ((i:int,_),(j,_)) => i>j)
in fun sortdups(cexp,dups) =
       map #2 (sort (map (fn dup as (v,w) => (nextuse v cexp, dup)) dups))
end

(* should do the first n and then only go 
   deep enough to prove that it is not needed *)

fun next_n_dups(0,cexp,dups) = []
  | next_n_dups(n,cexp,dups) =
    if n >= length dups
    then dups
    else cut(n,sortdups(cexp,dups))

fun show (SWITCH(v,l)) = (print "SWITCH "; sayv v; print "\n")
  | show (RECORD(_,w,_)) = (print "RECORD "; print w; print "\n")
  | show (SELECT(_,_,w,_)) = (print "SELECT "; print w; print "\n")
  | show (OFFSET(_,_,w,_)) = (print "OFFSET "; print w; print "\n")
  | show (PRIMOP(_,_,w::_,_)) = (print "PRIMOP "; print w; print "\n")
  | show (PRIMOP(_,vl,_,_)) = (print "PRIMOP "; vallist vl; print "\n")
  | show (APP(f,vl)) = (print "APP "; sayv f; vallist vl; print "\n")

    local fun vars(l, VAR x :: rest) = vars(x::l, rest)
            | vars(l, _::rest) = vars(l,rest)
            | vars(l, nil) = uniq l
       in fun clean l = vars(nil, l)
      end

nonfix before
val \/ = merge and /\ = intersect
infix 6 \/   infix 7 /\

fun spill(carg,prof) =
let 
    val freevars = 
	let exception SpillFreemap
	    val m = Intmap.new(32, SpillFreemap) : lvar list Intmap.intmap
	    val _ = app (freemap (Intmap.add m) o #3 o #1) carg
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
      | PRIMOP(i,args,[],l) => app check l
      | PRIMOP(i,args,w::_, l) => (checkv w; app check l)

  fun f(results : lvar list,
	  uniques : lvar list,
          dups : (lvar*lvar) list,
 	  spill : (lvar list * value) option,
	  cexp : cexp) =
    let val (before,after) =  (* variables free in this operation, and after
	  			  not including the newly-bound variables *)
	 let val rec free =
	      fn SWITCH(v,l) => foldmerge(clean[v] :: map free l)
	       | RECORD(l,w,c) =>  clean (map #1 l) \/ freevars w
	       | SELECT(i,v,w,c) => clean[v] \/ freevars w
	       | OFFSET(i,v,w,c) => clean[v] \/ freevars w
	       | PRIMOP(i,a,[],cl) => foldmerge(clean a :: map free cl)
	       | PRIMOP(i,a,[res],cl) => clean a \/ freevars res
	       | APP(f,vl) => clean(f::vl)
	   in case cexp
	      of SWITCH(v,l) => (clean[v], foldmerge(map free l))
	       | RECORD(l,w,c) =>  (clean(map #1 l), freevars w)
	       | SELECT(i,v,w,c) => (clean[v], freevars w)
	       | OFFSET(i,v,w,c) => (clean[v], freevars w)
	       | PRIMOP(i,a,[],cl) => (clean a, foldmerge(map free cl))
	       | PRIMOP(i,a,[res],cl) => (clean a, freevars res)
	       | APP(f,vl) => (clean(f::vl), [])
	 end

        val uniques = uniques \/ results
	val uniques_after = uniques /\ after
        val uniques_before = (uniques /\ before) \/ uniques_after
        val spill_after = 
	    case spill of
	      NONE => NONE
	    | SOME(contents,_) =>
	      case uniq contents /\ after of
	        [] => NONE
	      | _ => spill
	val maxfree' = case spill of NONE => maxfree | SOME _ => maxfree-1
	val maxfreeafter = case spill_after of
			     NONE => maxfree | SOME _ => maxfree-1
	val avail = maxfree' - length(uniques_before \/ results)
	val dups = next_n_dups(avail,cexp,dups)

        fun getpath p (VAR v) =
	  if member uniques_before v
	  then (VAR v, OFFp 0)
	  else let fun try((w,x)::l) = if v=w then (VAR x, OFFp 0) else try l
		     | try [] = let val SOME (l,sv) = spill
			            fun find(i,w::l) = 
				        if v=w
				        then (sv, SELp(i,OFFp 0))
				        else find(i+1,l)
				      | find(_,[]) = error "not found in spill"
			        in find(0,l)
			        end
	       in try dups
	       end
	  | getpath _ x = (x, OFFp 0)

	fun makeSpillRec args =
	    let val contents = clean args \/ after
	        val spillrec = map (getpath true o VAR) contents
		val sv = spillLvar()
		val dups' = map (fn x => (x,x)) uniques_before @ dups
	     in RECORD(spillrec,sv,f([],[],dups',SOME(contents, VAR sv),cexp))
	    end

        fun g(args,res,conts,gen) = 
	if length(clean args \/ uniques_after) > maxfreeafter orelse
	   length res + length uniques_after > maxfreeafter
	then makeSpillRec args
	else let val paths = map (fn x => (x, getpath false (VAR x))) (clean args)
		 fun fetchit (_,(_,OFFp 0)) = false | fetchit _ = true
	     in case sublist fetchit paths of
		  [(v,(w,SELp(i,OFFp 0)))] =>
		  let val x = dupLvar v
 		  in (* print "Fetching ";
		     print v;
		     print "\n"; *)
		     SELECT(i,w,x,f([],uniques_before,(v,x)::dups,
		     	            spill_after,cexp))
		  end
		| (v,(w,SELp(i,OFFp 0)))::_ =>
		  let val x = dupLvar v
 		  in (* print "fetching ";
		     print v;
		     print "\n"; *)
		     SELECT(i,w,x,f([],uniques_before,(v,x)::dups,spill,cexp))
		  end
	        | [] => let fun f' cexp = f(uniq res,uniques_after,
					    dups,spill_after,cexp)
		        in gen(map (#1 o (getpath false)) args,res,map f' conts)
			end
	     end

     in case ((*show cexp;*) cexp)
         of SWITCH(v,l) => g([v],[],l,fn([v],[],l)=>SWITCH(v,l))
          | RECORD(l,w,c) =>
	    if 1+length uniques_after > maxfreeafter
	    then makeSpillRec (map #1 l)
	    else let val paths = map (fn (v,p) =>
					 let val (v',p') = getpath true v 
					 in (v', combinepaths(p',p))
					 end)
	                             l
	         in RECORD(paths,w,f([w],uniques_after,dups,spill_after,c))
		 end
          | SELECT(i,v,w,c) => g([v],[w],[c], fn([v],[w],[c])=>SELECT(i,v,w,c))
	  | OFFSET(i,v,w,c) => g([v],[w],[c], fn([v],[w],[c])=>OFFSET(i,v,w,c))
	  | PRIMOP(i,a,r,cl) => g(a,r,cl, fn(a,r,cl)=>PRIMOP(i,a,r,cl))
	  | APP(f,vl) => g(f::vl,[],[],fn(f::vl,[],[])=>APP(f,vl))
   end

in map
   (fn (arg as ((func,vl,body),k)) =>
       ((check body; arg)
        handle TooMany => ((func,vl,f([],uniq vl, [],NONE,body)), k)))
   carg
end

end (* structure Spill *)
