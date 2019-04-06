(****************************************************************************
 * NOTE: debugging code has been commented out as follows:                  *
 *           (***> ...code... <***)                                         *
 *                                                                          *
 * The main function is closeCPS, whose type is approximately:              *
 *  val closeCPS : (cexp * profiling hook) ->                               *
 *                   (converted cexp * knownfunc?)                          *
 *                                                                          *
 ****************************************************************************)

signature CLOSURE =
  sig
    val closeCPS : CPS.function * (int * int * CPS.cexp -> CPS.cexp) ->
			CPS.function * (CPS.lvar -> bool)
  end

functor Closure(val maxfree : int) : CLOSURE =
struct

open CPS Access Profile SortedList
structure CGoptions = System.Control.CG

val OFFp0 = OFFp 0

(* miscellaneous *)
fun partition f l = fold (fn (e,(a,b)) => if f e then (e::a,b) else (a,e::b))
			 l ([],[])
fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl [] = []
  in  subl
  end
fun formap f =
  let fun iter([],_) = []
	| iter(hd::tl,i) = f(hd,i)::iter(tl,i+1)
  in  iter o (fn l => (l,0))
  end

val error = ErrorMsg.impossible
local val save = (!saveLvarNames before saveLvarNames := true)
      val closure = namedLvar(Symbol.symbol "closure")
in    val closureLvar = (saveLvarNames := save; fn () => dupLvar closure)
end

(* environment stuff *)
datatype object = Value
		| Function of {label:lvar,free:lvar list}
		| Closure of {functions : (lvar * lvar) list,
			      contents : (lvar * object) list,
			      offset : int,
			      stamp : lvar}
datatype env = Env of (lvar * object) list
datatype access = Direct
		| Path of (lvar * object * accesspath)

fun mkClosure(functions,contents) =
     Closure{functions=functions,contents=contents,
	     offset=0,stamp=mkLvar()}
fun augment(m,Env e) = Env (m::e)

(* return the ith object of a closure *)
fun select(i,Closure{functions,contents,offset,stamp}) =
     (let val index = offset + i - length functions
      in  (#2 o nth)(contents,index)
      end handle Nth => error "bad select in cps/closure")
  | select(_,Value) = Value
  | select(_,Function _) = error "select from knownfunc in cps/closure"

(* Bind the offset of a closure to a variable in an environment *)
fun offset(_,Value,_,_) = error "offset from value in cps/closure"
  | offset(_,Function _,_,_) = error "offset from knownfunc in cps/closure"
  | offset(i,Closure{functions,contents,offset,stamp},v,env) =
      augment((v,Closure{functions=functions,contents=contents,
			 offset=offset+i,stamp=stamp}),env)

(* Environment printing *)
val pr = outputc std_out
val vp = pr o Access.lvarName
fun plist p l = (app (fn v => (pr " "; p v)) l; pr "\n")
val ilist = plist vp
fun sayv(VAR v) = pr(Access.lvarName v)
  | sayv(INT i) = pr(makestring i)
  | sayv(REAL r) = pr r
  | sayv(STRING s) = (pr "\""; pr s; pr "\"")
val vallist = plist sayv

fun printEnv(Env e) =
  let fun ip i = pr(Integer.makestring i)
      fun sp() = pr " "
      val tlist = plist (fn (a,b) => (vp a; pr "/L"; Integer.print b))
      fun p(indent,l,seen) =
	let fun v(true,(vl,Value)::tl) = (vp vl; sp(); v(true,tl))
	      | v(false,(vl,Value)::tl) = (indent(); vp vl; sp(); v(true,tl))
	      | v(nl,_::tl) = v(nl,tl)
	      | v(true,[]) =  pr "\n"
	      | v(false,[]) = ()
	    fun f((v,Function{label,free})::tl) =
		(indent(); vp v; pr "/k"; vp label; pr " -";
		 ilist free; f tl)
	      | f(_::tl) = f tl
	      | f nil = ()
	    fun c(v,Closure{functions,contents,offset,stamp}) =
	       (indent(); pr "Closure "; vp v; pr "/"; ip stamp;
		pr " @"; ip offset;
		if member seen stamp
		then pr "(seen)\n"
		else (pr ":\n";
		      case functions of
		        [] => ()
		      | _ => (indent(); pr "  Funs:"; tlist functions; ());
		      p(fn() => (indent();pr "  "),contents,enter(stamp,seen))))
	      | c _ = ()
	in v(false,l); f l; app c l
	end
  in  p(fn () => (),e,[])
  end

(* Lookup.  Assumes target is not a constant. *)
exception Lookup of lvar * env
fun lookup(env as Env e,target) =
    let fun bfs([],[],seen) = raise Lookup(target,env)
	  | bfs([],next,seen) = bfs(next,[],seen)
	  | bfs((Closure{functions,contents,offset,stamp},p)::m,next,seen) =
	    let val (sawit,seen) = if member seen stamp
				   then (true,seen)
				   else (false,enter(stamp,seen))
		fun element i =
		 let val p' = i-offset
		 in
(***>
		     if p'<0
		     then (print "\nNegSel target for ";
			   print(Access.lvarName target); print " in\n";
			   printEnv env)
		     else ();
<***)
		     p'::p
		 end
		fun cnt([],i,next) = bfs(m,next,seen)
		  | cnt((v,c as Closure{stamp,...})::t,i,next) =
		      if target=v then (element i,0,c)
		      else cnt(t,i+1,if member seen stamp then next
				     else (c,element i)::next)
		  | cnt((v,Value)::t,i,next) =
		      if target=v then (element i,0,Value)
		      else cnt(t,i+1,next)
		  | cnt((_,Function _)::_,_,next) =
		      error "Function in closure in lookup"
		fun fns([],i) = cnt(contents,i,next)
		  | fns((v,l)::t,i) =
		      if target=v
		      then (p,i-offset,
			    Closure{functions=functions,contents=contents,
				    stamp=stamp,offset=i})
		      else fns(t,i+1)
	    in if sawit then bfs(m,next,seen)
	       else fns(functions,0)
	    end
	  | bfs _ = error "unexpected 23423222 in cps/closure/lookup/bfs"
	fun search closures =
	      let val (p,off,r) =
		      bfs(formap(fn((v,c),i) => (c,[i])) closures,[],[])
		  val (n::t) = rev p
		  fun f [] = OFFp off | f(h::t) = SELp(h,f t)
		  val (v,c) = nth(closures,n)
	      in  (r,Path(v,c,f t))
	      end
	fun look [] = raise Lookup(target,env)
	  | look ((v,c as Closure{functions,contents,stamp,offset})::tl) =
	      if target=v then (c,Direct)
	      else let fun f(_,[]) = NONE
			 | f(i,(v,_)::t) = if target=v then SOME i
					   else f(i+1,t)
		       (* this junk is a hack needed for linked closures *)
		   in  case f(0,functions)
			 of NONE => look tl
			  | SOME n =>
				(Closure{functions=functions,contents=contents,
					 stamp=stamp,offset=n},
				 Path(v,c,OFFp(n-offset)))
		   end
	  | look ((v,f as Function _)::tl) =
	      if target=v then (f,Direct) else look tl
	  | look ((v,Value)::tl) =
	      if target=v then (Value,Direct) else look tl
        fun isClosure (v,Closure _) = true 
	  | isClosure _ = false
    in  look e handle Lookup _ => search (sublist isClosure e)
    end

(****************************************************************************
 * "Alpha conversion": the closure converter introduces duplicate bindings  *
 * at function arguments (the free variables of known functions) and at     *
 * SELECT's and OFFSET's from closures.  This function restores unique      *
 * bindings, and also eliminates OFFSET's of 0 (which are introduced as     *
 * a side effect of trying to improve lazy display).  It assumes that a     *
 * FIX has no free variables.                                               *
 ****************************************************************************)
fun unrebind ce =
let fun rename rebind(VAR v) =
	      let fun f [] = VAR v
		    | f ((w:int,v')::t) = if v=w then v' else f t
	      in  f rebind
	      end
      | rename _ x = x
    fun f (l,args,b) =
      let val (args',rebind') = fold (fn(v,(args',rebind')) =>
					let val v' = dupLvar v
					in  (v'::args',(v, VAR v')::rebind')
					end)
				     args ([],[])
      in  (l,args',g(b,rebind'))
      end
    and g(ce, rebind: (lvar * value) list) =
      let val rename = rename rebind
	  val rec h =
	       fn RECORD(vl,w,e) =>
		    RECORD(map (fn(v,p) => (rename v,p)) vl,w,h e)
		| OFFSET(0,v,w,e) => g(e,(w,rename v)::rebind)
		| OFFSET(i,v,w,e) =>
			let val w' = dupLvar w
			in  OFFSET(i,rename v,w',g(e,(w, VAR w')::rebind))
			end
		| SELECT(i,v,w,e as APP(VAR x, args)) =>
			let val w' = dupLvar w
			in  if w=x
			    then SELECT(i,rename v,w',APP(VAR w',map rename args))
			    else SELECT(i,rename v,w',g(e,(w, VAR w')::rebind))
			end
		| SELECT(i,v,w,e) =>
			let val w' = dupLvar w
			in  SELECT(i,rename v,w',g(e,(w, VAR w')::rebind))
			end
		| APP(f,vl) => APP(f,map rename vl)
			(* HACK: f is always a label or from a SELECT, so
			   we never need rename. *)
		| FIX(l,e) => FIX(map f l,h e)
		| SWITCH(v,el) => SWITCH(rename v,map h el)
		| PRIMOP(i,vl,wl,el) => PRIMOP(i,map rename vl,wl,map h el)
      in  h ce
      end
in  g(ce,[])
end


(****************************************************************************
 * regf: Merge the free variables of recursive register functions.  If a    *
 * function calls another that needs the closure (for calling one of the    *
 * escaping functions) then this must be marked.                            *
 ****************************************************************************)
type info = {v:lvar,fns:lvar list,other:lvar list,args:lvar list,
	  body:cexp,label:lvar,env:env,callc:bool}
fun regf bindings =
let fun pack m =
      let fun getother w =
	    let fun g(({v,...}:info,other,_)::tl) = if v=w then other
			else g tl
                      | g [] = ErrorMsg.impossible "[] 4849 in cps/closure"
	    in  g m
	    end
	  fun getcallc w =
	    let fun g(({v,...}:info,_,callc)::tl) = if v=w then callc
			else g tl
                      | g [] = ErrorMsg.impossible "[] 4848 in cps/closure"
	    in  g m
	    end
	  fun f (x as {args,fns,...}:info, other, callc) =
		  (x,
		   foldmerge(other :: map getother fns),
		   callc orelse
		   (length args + length other >= maxfree andalso
		    length other > 1) orelse
		   exists getcallc fns)
	  val m' = map f m
      in  if exists (fn ({callc,...}:info,_,callc') => callc <> callc') m'
	  then regf (map (fn ({v,fns,other,args,body,label,callc,env},_,cc') =>
				{v=v,fns=fns,other=other,args=args,body=body,
				 label=label,env=env,callc=cc'}) m')
	  else if exists (fn x=>x) 
			(List2.map2 (fn ((_,other,_),(_,other',_)) => 
				length other <> length other')
			      (m,m'))
	       then pack m'
	  else fold (fn(({v,args,body,label,env,...},other,callc),(b,f)) =>
			if callc then 
			({v=v,args=args,body=body,label=label,env=env,
			 free=[],callc=callc}::b,merge(other,f))
			else
			({v=v,args=args,body=body,label=label,env=env,
			 free=other,callc=callc}::b,f))
		     m' ([],[])
      end
in  pack (map (fn (x as {other,callc,...}) => (x,other,callc)) bindings)
end

(* Closure strategy stuff.  Rudimentary. *)
fun flat(env,free) =
 map (fn v => let val (obj,_) = lookup(env,v)
	      in  (v,obj)
	      end) free
fun link(env,free) =
  let val contents = map (fn v => let val (obj,acc) = lookup(env,v)
				  in  (v,obj,acc)
				  end)
			 free
      val direct = fold (fn ((v,obj,Direct),t) => (v,obj)::t
			  | ((v,obj,Path(_,_,OFFp _)),t) => (v,obj)::t
			  | (_,t) => t) contents []
  in  if length direct = length contents then direct
      else case env of Env l =>
	      let fun getc ((m as (v,Closure _))::_) = m
		    | getc (_::tl) = getc tl
		    | getc [] = error "No closure in closureStrat"
		  val c = getc (rev l)
	      in  c::direct
	      end
  end
fun closureStrategy(bindings,free,env) = (* temporary *)
  let val m = case !CGoptions.closureStrategy
		of 3 => link(env,free)
		 | 2 => link(env,free)
		 | _ => flat(env,free)
  in  mkClosure(map (fn(v,l,_,_) => (v,l)) bindings,m)
  end



(* main function *)
fun closeCPS((f,vl,ce),prof) =
let
val (freevars,escapes,knownlabs) = FreeMap.freemapClose ce
fun clookup(e, VAR v) = lookup(e,v)
  | clookup(e, x) = (Value, Direct)
(***>
val alphac = System.Control.CG.alphac
val unrebind = fn x => if !alphac then unrebind x else x
<***)



(****************************************************************************
 * Take a free variable list and replace knownfuncs by their free           *
 * variables.  A new environment with the knownfunc mappings is returned.   *
 * Function aliasing could be added here.                                   *
 ****************************************************************************)
fun funcAnalysis(free,env) =
  fold (fn (v,(l,env')) =>
	let val(obj,_) = lookup(env,v)
	in  case obj
	      of Function{free,...} => (merge(free,l),augment((v,obj),env'))
	       | _ => (enter(v,l),env')
	end)
	free ([],Env [])
(* Function aliasing, separate for now, always called after funcAnalysis. *)
fun sameClosureOpt(free,env) =
case !CGoptions.closureStrategy
  of 0 => free (* flat without aliasing *)
   | 2 => free (* linked without aliasing *)
   | _ => (* all others have aliasing *)
  let val mapping = map (fn v => let val (obj,_) = lookup(env,v)
				 in  (v,obj)
				 end) free
      fun uniq ((hd as (v,Closure{stamp,...}))::tl) =
	let val m' = uniq tl
	in  if exists (fn (_,Closure{stamp=stamp',...}) => stamp=stamp'
			| _ => false) m'
	    then m' else hd::m'
	end
	| uniq (hd::tl) = hd::uniq tl
	| uniq [] = []
  in  map #1 (uniq mapping)
  end

(****************************************************************************
 * fixAccess: find the access path to a variable.  A header to select the   *
 * variable from the environment is returned, along with a new environment  *
 * that reflects the actions of the header (this last implements a "lazy    *
 * display").  fixAccess actually causes rebindings -- the variable         *
 * requested is rebound if it is not immediately available in the           *
 * environment.                                                             *
 ****************************************************************************)
fun fixAccess(args,env) =
let
fun access(VAR rootvar, (env,header)) =
  let val rec follow =
	fn (_,Value,_,_,_) => error "fixAccess Value in cps/closure"
	 | (v,cl,env,OFFp off,h) =>
		  (offset(off,cl,rootvar,env),
		   h o (fn ce => OFFSET(off,VAR v,rootvar,ce)))
	 | (v,cl,env,SELp(i,OFFp 0),h) =>
		  (augment((rootvar,select(i,cl)),env),
		   h o (fn ce => SELECT(i,VAR v,rootvar,ce)))
	 | (v,cl,env,SELp(i,p),h) =>
		  let val w = mkLvar()
		      val cl = select(i,cl)
		      val env = augment((w,cl),env)
			(* turn off lazy display here *)
		  in  follow(w,cl,env,p,h o (fn ce => SELECT(i,VAR v, w, ce)))
		  end
      val (_,acc) = lookup(env,rootvar)
  in  case acc
	of Direct => (env,header)
	 | Path(start,cl,path) =>
	     let val a as (env,header) = follow(start,cl,env,path,header)
	     in  if not(!CGoptions.profile) then a
		 else let val cost = lenp path
			  val h = if cost=0 then fn x => x else
			      if cost < LINKSLOTS
			      then fn ce => prof(LINKS+cost,1,ce)
			      else fn ce => prof(LINKS,1,prof(LINKOVFL,cost,ce))
		      in  (env,h o header)
		      end
	     end
  end
  | access(_, y) = y
in  fold access args (env,fn x => x)
end

(****************************************************************************
 * recordEl: Find the complete access paths for elements of a record.       *
 * Return a header for profiling purposes if needed.                        *
 ****************************************************************************)
fun recordEl(l,env) =
if not(!CGoptions.profile)
then (map (fn (v,p) => 
       		 case clookup(env,v)
		  of (_,Direct) => (v, p)
		   | (_,Path(start,_,path)) => (VAR start, combinepaths(path,p)))
	 l,
      fn x => x)
else fold (fn ((v,p),(l,h)) =>
	  let val (_,acc) = clookup(env,v)
	      val (m,cost) = case acc of Direct => ((v,p),0)
				| Path(start,_,path) =>
					((VAR start, combinepaths(path,p)),lenp path)
	      val h' = if cost=0 then fn x => x else
		      if cost < LINKSLOTS then fn ce => prof(LINKS+cost,1,ce)
		      else fn ce => prof(LINKS,1,prof(LINKOVFL,cost,ce))
	  in  (m::l,h o h')
 	 end)
	 l 
	([],fn x => x)


(* Create the environments for functions in a FIX. *)
fun makenv(initEnv,bindings: (lvar * lvar list * cexp) list) =
let
(***>
    fun COMMENT f = if !System.Control.CG.comment then (f(); ()) else ()
    val _ = COMMENT(fn() => (pr "BEGINNING MAKENV.\nFunctions: ";
			     ilist (map #1 bindings);
			     pr "Initial environment:\n";
			     printEnv initEnv; pr "\n"))
(* A debugging version of freevars *)
val freevars =
  (fn v => let val foo as (_,free) = freevars v
	   in  COMMENT(fn() => (pr "Free in "; vp v; pr ":"; ilist free));
	       foo
	   end)
<***)

(* Separate functions into those that escape and those which are knownfuncs *)
val (escape,known) = partition (escapes o #1) bindings
val escaping = uniq(map #1 escape)

(***>
val _ = COMMENT(fn() => (pr "Knownfuncs:"; ilist (map #1 known)))
<***)

(* Mark each known function of the FIX with its free variables and label. *)
val known = map (fn(v,args,body) =>
		  let val (label,free) = freevars v
		  in  {v=v,free=free,args=args,body=body,label=label}
		  end)
                known

(* For each known function of the FIX, remove any escaping functions of the
   FIX from its free list and mark that the function requires the closure. *)
val known
	= map (fn {v,free,args,body,label} =>
		let val free' = difference(free,escaping)
		in  {v=v,free=free',callc=(free<>free'),
		     args=args,body=body,label=label}
		end) known

(* Separate known functions defined in this FIX from other free variables. *)
local val knownlvars = map #v known
in    val knownlvar = fn v => exists (fn w => v=w) knownlvars
end
val known
	= map (fn {v,free,callc,args,body,label} =>
		let val (fns,other) = partition knownlvar free
    		in  {v=v,fns=fns,other=other,callc=callc,
		     args=args,body=body,label=label}
		end)
	      known

(* Replace knownfuncs defined in other FIX'es by their free variables, and
   escaping functions defined in other FIX'es by their closures.
   The environment returned by funcAnalysis is saved because
   it has knownfunc bindings (and only knownfunc bindings). *)
val known
	= map (fn{v,fns,other,callc,args,body,label} =>
		let val (other,env') = funcAnalysis(other,initEnv)
		    val other = sameClosureOpt(other,initEnv)
		in  {v=v,fns=fns,other=other,callc=callc,args=args,body=body,
		     env=env',label=label}
		end)
	      known

(*
   Merge free variables of knownfuncs that call each other.  Look at the
   number of free variables and arguments to each known function to be
   defined.  The cps converter ensures that there are enough registers to
   hold the arguments and leaves one register free for the free
   variables, if any.  Therefore some free variables may have to be
   spilled into the closure, and these must be collected.  Also see if
   adding the immediately enclosing closure will help.
*)
val immed = (* The immediately enclosing closure, if any *)
  let fun scan nil = NONE
	| scan ((e as (v,Closure _))::_) = SOME(v,Env [e])
	| scan (_::tl) = scan tl
      val Env e = initEnv
  in  scan e
  end
val known = map (fn {v,fns,other,args,body,env,label,callc} =>
		 let val other =
		       case immed of NONE => other
		     | SOME(immedv,e) => 
			 let fun inImmed v =
			       (lookup(e,v); true) handle Lookup _ => false
			     val immedc = exists inImmed other
			 in  if immedc
			     then enter(immedv,sublist (not o inImmed) other)
			     else other
			 end
		 in  {v=v,fns=fns,other=other,args=args,body=body,
		      env=env,label=label,callc=callc}
		 end) known
val (known, collected) = regf known

(***>
val _ = COMMENT(fn() => (pr "Escaping functions:"; ilist (map #1 escape)))
<***)
(* Get the combined list of the free variables of all the escaping functions
   of the FIX, and the labels for each function *)
val (free,escape) =
 let val (f,e) = fold (fn ((v,a,b),(f',e')) =>
		         let val (l,f'') = freevars v
			 in  (merge(f'',f'),(v,l,a,b)::e')
			 end)
                      escape (nil,nil)
 in  (remove(escaping, f),e)
 end

(* Replace knownfuncs defined in this FIX with their free variables. *)
val free : lvar list
	= let val (fns,other) = partition knownlvar free
	  in  fold (fn ({v,free,...},b) =>
			if exists (fn w => v=w) fns
			then merge(free,b)
			else b) known other
	  end

(* Add the free variables of knownfuncs in this FIX which were spilled into
   the closure. *)
val free = merge(collected,free)

(* Replace knownfuncs defined elsewhere with their free variables, and escaping
   functions defined elsewhere with their closures.  The function environment
   which tells that certain free variables are known functions and gives their
   free variables must be kept for applications of the functions in the bodies
   of the escaping functions of the FIX. *)
val (free,functionEnv) : lvar list * env (* only need function mapping here *)
	= let val (free,env') = funcAnalysis(free,initEnv)
	      val free = sameClosureOpt(free,initEnv)
	  in  (free,env')
	  end

(* Given the functions to be defined in the closure (escape), the free
   variables which should be contained in the closure (free), and their
   current locations (env), decide on a closure representation. *)
val closure = closureStrategy(escape,free,initEnv)

fun mkFnMap c : (lvar * object) list
	= map (fn{v,free,callc,label,...} =>
		if callc then (v,Function{label=label,free=enter(c,free)})
		else (v,Function{label=label,free=free}))
	      known

(* Final construction of the environment for each standard function. *)
val closureFrags : (lvar * lvar list * cexp * env) list
	= case escape of [] => []
	| ((v,_,_,_)::_) =>
	  let val env = fold augment (mkFnMap v) functionEnv
	      fun f ((v,l,args,body),i) =
		let val cname = closureLvar()
		    val env = fold (fn (v,b) => augment((v,Value),b))
				args (offset(i,closure,cname,env))
(***>
		    val _ = COMMENT(fn () => (print "\nEnvironment in escaping ";
					      vp v; print ":\n";
					      printEnv env))
<***)
		in  (l,cname::args,body,env)
		end
	  in  formap f escape
	  end

(* Final construction of the environment for each known function. *)
val cname = closureLvar()
val fnMap = mkFnMap cname
val registerFrags : (lvar * lvar list * cexp * env) list
	= map (fn{v,free,callc,args,body,env=env',label} =>
		let val env =
		      fold (fn (v,env') =>
				case lookup(initEnv,v)
				  of (Function _,_) => error "cps/closure.223"
				   | (obj,_) => augment((v,obj),env'))
			   free
			   (fold (fn (v,b) => augment((v,Value),b))
				 args
				 (fold augment fnMap
				    (if callc
				     then (inc System.Control.CG.knowncl;
					   augment((cname,closure),env'))
			  	     else env')))
(***>
		    val _ = COMMENT(fn () => (print "\nEnvironment in known ";
					      vp v; print ":\n";
					      printEnv env))
<***)
		    val args = args @ free @ if callc then [cname] else []
		in  (label,args,body,env)
		end)
	      known


val contents = let val Closure{functions,contents,...} = closure
	       in  map (LABEL o #2) functions @ map (VAR o #1) contents
	       end


(* Add profiling code if flag is on. *)
fun mkrexp(contents,cname) =
  if not(!CGoptions.profile) then fn ce => RECORD(contents,cname,ce)
  else let val len = length contents
	   val (closures,slots,ovfl) =
		fold (fn((v,[_],_),b as (closures,_,_)) =>
			if closures=CLOSURES then b
			else if escapes v
			     then (CCLOSURES,CCLOSURESLOTS,CCLOSUREOVFL)
			     else b
		      |((v,args,_),b as (closures,_,_)) =>
			if closures=CLOSURES then b
			else if escapes v
			     then (CLOSURES,CLOSURESLOTS,CLOSUREOVFL)
			     else b)
		     bindings (KCLOSURES,KCLOSURESLOTS,KCLOSUREOVFL)
       in  if len < slots
	   then fn ce => prof(closures+len,1,RECORD(contents,cname,ce))
	   else fn ce => prof(closures,1,
			      prof(ovfl,len,RECORD(contents,cname,ce)))
       end


in  case contents
      of [] => (fn ce => ce,registerFrags,fold augment fnMap initEnv)
       | _ =>
	  let val frags = closureFrags@registerFrags
	      val env = fold (fn(a,b) => augment((#1 a,Value),b))
		             closureFrags initEnv
	      val (contents,header) = 
		recordEl(map (fn v => (v, OFFp0)) contents, env)
	      val env = fold augment fnMap env
	      val env = augment((cname,closure),env)
	  in
(***>
	      COMMENT(fn () => (print "\nEnvironment after FIX:\n";
	                        printEnv env; pr "MAKENV DONE.\n\n"));
	      
<***)
	      (header o mkrexp(contents,cname),frags,env)
	  end
end


val env1 = fold (fn(v,b) => augment((v,Value),b)) (f::vl) (Env [])
fun close(ce,env) =
  case ce
    of FIX(bindings,b) =>
       (let val (header,frags,env') = makenv(env,bindings)
	in  FIX(map (fn(v,args,a,env) =>
				(v,args,close(a,env))) frags,
		header(close(b,env')))
	end handle Lookup(i,e) => (print "LOOKUP FAILS in close(FIX";
				   ilist (map #1 bindings);
				   print ") on "; vp i;
				   print "\nin environment:\n";
				   printEnv e;
				   APP(VAR 0,nil)))
     | APP(f,args) =>
       (let val(obj,_) = clookup(env,f)
	in  case obj
	      of Closure{functions,offset,...} =>
		   let val (_,header) = fixAccess(f::args,env)
		       val (_,label) = nth(functions,offset)
		       val call = APP(LABEL label,f::args)
		   in  if !CGoptions.profile
		       then header(prof(STDKCALLS,1,call))
		       else header call
		   end
	       | Function{label,free} =>
		   let val args' = args@(map VAR free)
		       val (_,header) = fixAccess(args',env)
		       val call = APP(LABEL label,args')
		   in  if !CGoptions.profile
		       then header(prof(KNOWNCALLS,1,call))
		       else header call
		   end
	       | Value =>
		   let val l = mkLvar()
		       val (_,header) = fixAccess(f::args,env)
		       val call = SELECT(0,f,l,APP(VAR l,f::args))
		   in  if !CGoptions.profile
		       then case args
			      of [_] => header(prof(CNTCALLS,1,call))
			       | _ =>  header(prof(STDCALLS,1,call))
		       else header call
		   end
	end
	    handle Lookup(i,e) => (print "LOOKUP FAILS in close(APP ";
				   sayv f; print "("; vallist args;
				   print")) on "; vp i;
				   print "\nin environment:\n";
				   printEnv e;
				   APP(VAR 0,nil)))
     | SWITCH(v,l) =>
	let val (env',header) = fixAccess([v],env)
	in  header (SWITCH(v,map (fn c => close(c,env')) l))
	end
     | RECORD(l,v,c) =>
	let val (l,header) = recordEl(l,env)
	    val ce = close(c,augment((v,Value),env))
	    val len = length l
	in  header(
	    if not(!CGoptions.profile) then RECORD(l,v,ce)
	    else if len < RECORDSLOTS
	    then prof(RECORDS+len,1,RECORD(l,v,ce))
	    else prof(RECORDS,1,prof(RECORDOVFL,len,RECORD(l,v,ce))))
	end
     | SELECT(i,v,w,c) =>
       (let val (env,header) = fixAccess([v],env)
	    val (obj,_) = clookup(env,v)
	in  header(SELECT(i,v,w,close(c,augment((w,select(i,obj)),env))))
	end
		handle Lookup(i,e) => (print "LOOKUP FAILS in close(SELECT";
				       vallist [v,VAR w]; print ") on "; vp i;
				       print "\nin environment:\n";
				       printEnv e;
				       SELECT(0,VAR 0,0,APP(VAR 0,nil))))
     | OFFSET(i,v,w,c) => error "OFFSET in cps/closure.sml!"
     | PRIMOP(i,args,rets,l) =>
	let val (env,header) = fixAccess(args,env)
	    val env = fold (fn (v,b) => augment((v,Value),b)) rets env
	in  header (PRIMOP(i,args,rets,map (fn c => close(c,env)) l))
	end
in  ((mkLvar(),f::vl,unrebind(close(ce,env1))),
     knownlabs)
end

end (* structure Closure *)


