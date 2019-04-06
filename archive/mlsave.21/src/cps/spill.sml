(* Spilling:
   This is a simple spiller, which maintains one flat spill record
   when necessary.

   Traverse the cps tree, keeping track of what lvars are in registers.
   The register environment holds lvars, and possibly a spill record
   and lvars duplicated in the spill record.
   When you need to allocate a new lvar, choose a place for it:
	1. replace a dead lvar
	2. replace a record which exists for only one live variable
	   which is already duplicated.
	3. replace a live lvar which is duplicated in the spill record.
	4. replace a record which has been duplicated.
   Otherwise, you must spill.

   Right now, I assume that primops use no more registers than the number
   of values they return.

   I also assume that the register used to hold the spill record will not
   be modified until after the contents have been stored.

   Still left:

	Possible allocation of args and free variables into records for
	knownfuncs in closure.sml

	A register for selecting from the spill record need not be reserved
	in some cases not presently recognized.  For instance, if the dup
	list is not empty, we can use one of those records.

	Access of variables from the spill record is a bit unsophisticated;
	when a variable is pulled from the record into the select register,
	this should be noted for possible later use, say in another field
	of spillrec.  If the variable is still free and there is space, it
	should be placed on the dup list.
*)

signature SPILL =
  sig structure CPS : CPS
      val spill : (CPS.function * bool) list * (CPS.lvar -> bool)
				 * (int * int * CPS.cexp -> CPS.cexp) ->
			(CPS.function * bool) list
  end


functor Spill(val maxfree : int) : SPILL =
struct

structure CPS = CPS
open FreeMap Access Profile SortedList CPS
val error = ErrorMsg.impossible
fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl nil = nil
  in  subl
  end
(* Registers hold unique lvars, a spill record, and lvars duplicated
   in the spill record. *)
datatype regs = Reg of {unique:lvar list,
			spillrec:{name:lvar,
				  contents:lvar list,
				  duped:lvar list} option}

local val save = (!saveLvarNames before saveLvarNames := true)
      val spillrec = namedLvar(Symbol.new "spillrec")
in    val spillLvar = (saveLvarNames := save; fn () => dupLvar spillrec)
end

val ilist = PrintUtil.printClosedSequence
		      ("[",",","]")
		      (fn i => (Integer.print i; ()))
fun printRegs(Reg{unique,spillrec}) =
  (print "Unique:   "; ilist unique; print "\n";
   case spillrec
     of NONE => (print "No spillrec\n";())
      | SOME{contents,duped,...} =>
	  (print "Contents: "; ilist contents; print "\n";
           print "Duped:    "; ilist duped; print "\n"; ()))

fun spill(carg,constant,prof) =
  let fun path regs =
	fn v =>
	 (case regs
	    of Reg{spillrec=NONE,...} => (v,OFFp 0)
	     | Reg{unique,spillrec as SOME{name,contents,duped}} =>
		let fun f(nil,_) = error "free variable in spill.sml"
		      | f(h::t,i) = if v=h then SELp(i,OFFp 0) else f(t,i+1)
		in  if member unique v orelse member duped v orelse constant v
		    then (v,OFFp 0)
		    else (name,f(contents,0))
		end)
      fun fixAccess(args,regs) =
	let fun f(arg,(h,l)) =
	      let val(w,p) = path regs arg
		  val(h,l) =
	          case p
		   of OFFp 0 => (h,w::l)
		    | SELp(i,OFFp 0) =>
			let val arg' = dupLvar arg
			in  (h o (fn ce => SELECT(i,w,arg',ce)),arg'::l)
			end
		    | _ => error "Bad access path in spill.sml"
	      in  if not(!CGoptions.profile) then (h,l)
		  else let val cost = lenp p
			  val h' = if cost=0 then fn x => x else
			      if cost < SPLINKSLOTS
			      then fn ce => prof(SPLINKS+cost,1,ce)
			      else fn ce => prof(SPLINKS,1,prof(SPLINKOVFL,cost,ce))
		      in  (h' o h,l)
		      end
	      end
	in  case regs
	      of Reg{spillrec=NONE,...} => (fn x => x,args)
	       | _ => fold f args (fn x => x,nil)
	end
      fun assign(regs as Reg{unique,spillrec},free,create,keep) =
	let val Reg{unique,spillrec} = regs
	    val (unique,spillrec) =
	      case spillrec
		of NONE => (free,NONE)
		 | SOME{name,contents,duped} =>
		    let val duped = intersect(free,duped)
			val contents' = intersect(free,contents)
		    in  case (contents',duped)
			  of (nil,nil) => (intersect(free,unique),NONE)
			   | ([v],[w]) => (enter(v,intersect(free,unique)),NONE)
						(* v=w always *)
			   | _ => (intersect(free,unique),
				   SOME{name=name,contents=contents,duped=duped})
		    end
	    val (spilled,uspill) =
		    case spillrec
		      of NONE => (false,0)
		       | SOME{duped,...} => (true,length duped + 2)
	    val used = length unique + uspill
	    fun spillit() =
	      let val name = spillLvar()
	      val free = merge(uniq keep,free)
	      val l = map (path regs) free
	      val len = length l
	      val h' = if not(!CGoptions.profile) then fn ce => RECORD(l,name,ce)
		       else if len < SPILLSLOTS then
			fn ce => prof(SPILLS+len,1,RECORD(l,name,ce))
		       else fn ce => prof(SPILLS,1,
				     prof(SPILLOVFL,len,RECORD(l,name,ce)))
	      val duped = case unique
			    of _::(a as _::_::b) => if spilled then a else b
			     | _ => error "bad spill in spill.sml"
	      val spillrec = SOME{name=name,contents=free,duped=duped}
	      val regs' =Reg{unique=create,spillrec=spillrec}
	      in  (h',regs',regs')
	      end
            fun cut(nil,l) = l
	      | cut(_::t,_::l) = cut(t,l)
	      | cut(_,nil) = error "the unkindest cut of all"
	in  if used + length create <= maxfree
	    then (fn x => x,
		  regs,
		  Reg{unique=merge(create,unique),spillrec=spillrec})
	    else case spillrec
	           of NONE => spillit()
		    | SOME{name,contents,duped} =>
			if length create <= length duped
			then (fn x => x,
			      regs,
			      Reg{unique=merge(create,unique),
				  spillrec=SOME{name=name,
						contents=contents,
						duped=cut(create,duped)}})
			else spillit()
	end
      val freevars = freemapSpill(carg,constant)
      val nonconstant = sublist (not o constant)
      fun spiller regs =
	fn FIX _  => ErrorMsg.impossible "FIX in cps/spill"
	 | APP(l,args) =>
		(* The spiller can't fix things if the number of args is greater
		   than the number of registers. *)
		let val (h,args) = fixAccess(args,regs)
		in  h(APP(l,args))
		end
	 | SWITCH(v,l) =>
		let val (h,[v]) = fixAccess([v],regs)
		in  h(SWITCH(v,map (spiller regs) l))
		end
	 | RECORD(l,w,c) =>
		let val (h,regs,regs') =
			assign(regs,freevars w,[w],nonconstant(map #1 l))
		    val (h,l) =
			if not(!CGoptions.profile)
			then (h,
			      map (fn (v,p) => let val(v',p') = path regs v
					       in  (v',combinepaths(p',p))
					       end) l)
			else
			fold (fn ((v,p),(h,l)) =>
			  let val(v',p') = path regs v
			      val cost = lenp p'
			      val h' = if cost=0 then fn x => x else
					if cost < SPLINKSLOTS
					then fn ce => prof(SPLINKS+cost,1,ce)
					else fn ce => prof(SPLINKS,1,
							prof(SPLINKOVFL,cost,ce))
			  in  (h o h',(v',combinepaths(p',p))::l)
			  end) l (h,nil)
		in  h(RECORD(l,w,spiller regs' c))
		end
	 | SELECT(i,v,w,c) =>
		let val (h,regs,regs') =
			assign(regs,freevars w,[w],[v])
		    val (h',[v]) = fixAccess([v],regs)
		in  h(h'(SELECT(i,v,w,spiller regs' c)))
		end
	 | OFFSET(i,v,w,c) =>
		let val (h,regs,regs') =
			assign(regs,freevars w,[w],[v])
		    val (h',[v]) = fixAccess([v],regs)
		in  h(h'(OFFSET(i,v,w,spiller regs' c)))
		end
	 | PRIMOP(i,args,ret as w::_,l) =>
		let val (h,regs,regs') =
			assign(regs,freevars w,ret,nonconstant args)
		    val (h',args) = fixAccess(args,regs)
		in  h(h'(PRIMOP(i,args,ret,map (spiller regs') l)))
		end
	 | PRIMOP(i,args,nil,l) =>
		let val (h,args) = fixAccess(args,regs)
		in  h(PRIMOP(i,args,nil,map (spiller regs) l))
		end
  in  map (fn ((l,a,b),k) =>
	      ((l,a,spiller (Reg{unique=uniq a,spillrec=NONE}) b),k)) carg
  end

end (* structure Spill *)
