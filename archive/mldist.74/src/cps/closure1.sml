(* closure1.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories
 *)

(****************************************************************************
 * NOTE: debugging code has been commented out as follows:                  *
 *           (***> ...code... <***)                                         *
 *                                                                          *
 * The main function is closeCPS, whose type is approximately:              *
 *  val closeCPS : (cexp * profiling hook) ->                               *
 *                   (converted cexp)                                       *
 *                                                                          *
 ****************************************************************************)

signature CLOSURE =
  sig
    val closeCPS : CPS.function * (int * int * CPS.cexp -> CPS.cexp) ->
			CPS.function
  end

functor ClosureCallee(val maxfree : int) : CLOSURE =
struct

open CPS Access Profile SortedList
structure CGoptions = System.Control.CG

val OFFp0 = OFFp 0

(******************** miscellaneous *****************************************)

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
fun clean l = 
let fun vars(l, VAR x :: rest) = vars(x::l, rest)
      | vars(l, _::rest) = vars(l,rest)
      | vars(l, nil) = l
 in vars(nil,l)
end
fun in'(v,nil) = false
  | in'(v,hd::tl) = if v = hd then true else in'(v,tl)

val error = ErrorMsg.impossible
local val save = (!saveLvarNames before saveLvarNames := true)
      val closure = namedLvar(Symbol.varSymbol "closure")
in    val closureLvar = (saveLvarNames := save; fn () => dupLvar closure)
end

fun extraLvar (k) = if k = 0 then nil 
                    else ((mkLvar())::(extraLvar(k-1)))
fun extraConst (k) = if k = 0 then nil 
                     else ((INT 0)::(extraConst(k-1)))


(********************* environment stuff ************************************)

datatype object = Value
		| Function of {label:lvar,free:lvar list}
		| Closure of {functions : (lvar * lvar) list,
			      contents : (lvar * object) list,
			      offset : int,
			      stamp : lvar}
datatype env = Env of (lvar * object) list
datatype access = Direct
		| Path of (lvar * object * accesspath)

(*** cont environment is a hash table during the whole transformations ******)
datatype cont = Dummy
              | Outside of value list
              | Inside of {label : lvar ,
                           extra : (value * object) list}

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
  | sayv(LABEL v) = (pr "(L)"; pr(Access.lvarName v))
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

(* 
fun write s = 
        (if !System.Control.CG.printit then () else outputc std_out s)
fun fprint (function as (f,vl,cps)) =
	(if !System.Control.CG.printit
              then ()	
              else CPSprint.show write
	        	(CPS.FIX([function],CPS.PRIMOP(Access.P.+,[],[],[])));
	 if !System.Control.CG.printsize then CPSsize.printsize cps else ())
 *)

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
(****>
		     if p'<0
		     then (print "\nNegSel target for ";
			   print(Access.lvarName target); print " in\n";
			   printEnv env)
		     else ();
<****)
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
       in (l,args',g(b,rebind'))
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
		| APP(f,vl) => APP(rename f,map rename vl)
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
	  body:cexp,label:lvar,env:env,callc:bool,extrac:bool}
fun regf'(bindings,tag) = 
let fun regf bindings =
  let fun pack m =
        let fun getother w =
	      let fun g(({v,...}:info,other,_,_)::tl) = if v=w then other
		 	else g tl
                    | g [] = ErrorMsg.impossible "[] 4849 in cps/closure"
	      in  g m
	      end
	    fun getcallc w =
	      let fun g(({v,...}:info,_,callc,_)::tl) = if v=w then callc
			else g tl
                    | g [] = ErrorMsg.impossible "[] 4848 in cps/closure"
	       in  g m
	      end
            fun getextrac w =
	      let fun g(({v,...}:info,_,_,extrac)::tl) = if v=w then extrac
			else g tl
                    | g [] = ErrorMsg.impossible "[] 4847 in cps/closure"
	       in  g m
	      end              
	    fun f (x as {args,fns,...}:info, other, callc, extrac) =
              let val sc = (length args + length other + tag >= maxfree) andalso
		           (length other > 1)
               in (x,
		   foldmerge(other :: map getother fns),
		   callc orelse (exists getcallc fns) 
                   orelse sc,
		   extrac orelse (exists getextrac fns))
              end
	    val m' = map f m
        in  if exists (fn ({callc,extrac,...}:info,_,callc',extrac')
                         => (callc <> callc') orelse (extrac <> extrac')) m'
	    then regf (map (fn ({v,fns,other,args,body,label,env,callc,
                                 extrac},_,cc',ec') 
                              =>  {v=v,fns=fns,other=other,args=args,body=body,
			           label=label,env=env,callc=cc',
                                   extrac=ec'}
                            ) m'
                      )
	    else if exists (fn x=>x) 
			(List2.map2 (fn ((_,other,_,_),(_,other',_,_)) => 
				length other <> length other')
			      (m,m'))
	         then pack m'
	    else fold (fn(({v,args,body,label,env,...},
                           other,callc,extrac),(b,f)) =>
			if callc then 
			 ({v=v,args=args,body=body,label=label,env=env,
			   free=[],callc=callc,
                           extrac=extrac}::b,merge(other,f))
			else ({v=v,args=args,body=body,label=label,env=env,
			       free=other,callc=callc,extrac=extrac}::b,f))
		       m' ([],[])
        end
  in  pack (map (fn (x as {other,callc,extrac,...}) => 
                          (x,other,callc,extrac)) bindings)
  end
in  regf(bindings)
end


(********************** Closure strategy stuff.  Rudimentary. **************)
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



(********************** main function ******************************************)
fun closeCPS((f,vl,ce),prof) =
let
 
val tag = !System.Control.CG.calleesaves
val tag = if (tag = 1) then 2 else tag     (* case tag = 1 is not well-defined *)
val tagConst = extraConst(tag)

val method = 0 (* or !System.Control.CG.misc3 *)

(*** elminate strange continuation variables ***)
val ((f,vl,ce),iscont0) = ContMap.contmap(f,vl,ce)

val (freevars,escapes,knowns) = FreeMap.freemapClose ce
fun clookup(e, VAR v) = lookup(e,v)
  | clookup(e, x) = (Value, Direct)

fun iscont v = (iscont0 v) andalso (not (knowns v))
fun iscontv (VAR v) = iscont v
  | iscontv _ = false 

exception Notcont
val cenv : cont Intmap.intmap = Intmap.new(32,Notcont)
fun findc c = (Intmap.map cenv c) 
                  handle Notcont => (print "unbounded variables : ";
				     print c ;
				     print " \n";
				     Dummy)
fun findextra (VAR c) = 
      (case (findc c) of
         Dummy => error "unbound cont vars 7328 "
       | Outside extra => (VAR c,extra)
       | Inside {label=label,extra=extra} => (LABEL label,(map #1 extra)))
  | findextra x = (x,tagConst)


fun outside v = case (findc v) of 
                  Outside _ => true
                | _ => false 
fun inside v = case (findc v) of 
                  Inside _ => true 
                | _ => false

fun addoutc(c,extra) = Intmap.add cenv (c,Outside extra)
fun addinc(c,label,extra) = Intmap.add cenv (c,Inside {label=label,extra=extra})

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
	       | _ => if iscont v then 
                        (let val (_,extra) = findextra (VAR v)
                             val extra = if outside v then enter(v,uniq(clean extra))
                                         else uniq(clean extra)
                             val env' = if inside v then augment((v,Value),env')
                                        else env'
                          in (merge(extra,l),env')
                         end)
                      else (enter(v,l),env')
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
fun makenv(initEnv,bindings: (lvar * lvar list * cexp) list,fvl) =
let

(*****> 
    fun COMMENT f = if !System.Control.CG.comment then (f(); ()) else ()
    val _ = COMMENT(fn() => (pr "BEGINNING MAKENV.\nFunctions: ";
			     ilist (map #1 bindings);
			     pr "Initial environment:\n";
			     printEnv initEnv; pr "\n"))
   A debugging version of freevars 
    val freevars =
         (fn v => let val foo as (_,free) = freevars v
	           in  COMMENT(fn() => (pr "Free in "; vp v; pr ":"; ilist free));
	               foo
	          end)
<******)

(* Separate functions into those that escape and those which are knownfuncs 
 * conts + escape + known = all funs in this fix.
 *)
val (escape,known) = partition (escapes o #1) bindings
val (contvar,escaping) = partition iscont (uniq(map #1 escape))
val (conts,escape) = partition (iscont o #1) escape



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
 * FIX from its free list and mark that the function requires the closure. 
 *)
val known
	= map (fn {v,free,args,body,label} =>
		let val free' = difference(free,escaping)
                    val cac = (free <> free')
		in  {v=v,free=free',callc=cac,
		     args=args,body=body,label=label}
		end) known

(* For each known function of the FIX, remove any continuation functions of the 
 * FIX from its free list and mark that the function requires the continuation
 * extra arguments 
 *)
val known 
	= map (fn {v,free,callc,args,body,label} =>
		let val free' = difference(free,contvar)
                    val erac = (free<>free')
		in  {v=v,free=free',callc=callc,extrac=erac,
		     args=args,body=body,label=label}
		end) known        

(* Separate known functions defined in this FIX from other free variables. *)
local val knownlvars = map #v known
in    val knownlvar = fn v => (exists (fn w => v=w) knownlvars)
end
val known
	= map (fn {v,free,callc,extrac,args,body,label} =>
		let val (fns,other) = partition knownlvar free
    		in  {v=v,fns=fns,other=other,callc=callc,extrac=extrac,
		     args=args,body=body,label=label}
		end)
	      known

(* Replace knownfuncs defined in other FIX'es by their free variables,
 * escaping functions defined in other FIX'es by their closures and 
 * continuation variables elsewhere by their extra arguments .
 * The environment returned by funcAnalysis is saved because
 * it has knownfunc bindings (and only knownfunc bindings). 
 *)
val known
	= map (fn{v,fns,other,callc,extrac,args,body,label} =>
		let val (other,env') = funcAnalysis(other,initEnv)
		    val other = sameClosureOpt(other,initEnv)
		in  {v=v,fns=fns,other=other,callc=callc,extrac=extrac,
                     args=args,body=body,env=env',label=label}
		end)
	      known

(* Merge free variables of knownfuncs that call each other.  Look at the
 * number of free variables and arguments to each known function to be
 * defined.  The cps converter ensures that there are enough registers to
 * hold the arguments and leaves one register free for the free
 * variables, if any.  Therefore some free variables may have to be
 * spilled into the closure, and these must be collected.  Also see if
 * adding the immediately enclosing closure will help.
 *)
fun scan nil = nil 
  | scan ((e as (v,Closure {functions,...}))::tl) = (v,Env [e])::(scan tl)
(*       (case functions of nil => (v,Env [e])::(scan tl)
                        | _ => (scan tl)) *)
  | scan (_::tl) = scan tl
val Env e = initEnv
val closlist = scan e  (* also used in calleealloc *)
val immed = case closlist 
              of nil => NONE 
               | hd::tl => SOME hd (* the immediately enclosing clos, if any *)

val known = 
   if (method = 1) then
           (map (fn {v,fns,other,callc,extrac,args,body,env,label} =>
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
		      label=label,env=env,callc=callc,extrac=extrac}
		 end) known)
   else known

(* regf' is still an experimental version , it's possible to add more accurate 
 * information into it so that it do things much better ! 
 * for all known functions, 
 * a. (callc,extrac) = (T,T) --- we need closures and calleesaveregs;
 * b. (callc,extrac) = (T,F) --- we need closures only;
 * c. (callc,extrac) = (F,F) --- we don't need closures
 *)
val known = map (fn {v,fns,other,args,body,label,env,callc,extrac} =>
                    {v=v,fns=fns,other=other,args=args,body=body,
                     label=label,env=env,callc=callc orelse extrac,
                     extrac=extrac}) known

val (known, collected) = regf'(known,tag)

(***> 
val _ = COMMENT(fn() => (pr "Escaping functions:"; ilist (map #1 escape)))
<***)


(* test if it's the inner most continuation function *)
val bmerge = fn ((x,xl),(y,yl)) => ((x orelse y),merge(xl,yl))
fun ebtest0(v,vl,body) = if iscont v then (true,FreeMap.freevars(body))
                         else ebtest(body)
and ebtest(APP _) = (false,nil)
  | ebtest(RECORD(vl,w,ce)) = ebtest(ce)
  | ebtest(SELECT(i,v,w,ce)) = ebtest(ce)
  | ebtest(OFFSET(i,v,w,ce)) = ebtest(ce)
  | ebtest(FIX(l,ce)) = fold bmerge (map ebtest0 l) (ebtest ce)
  | ebtest(SWITCH(v,l)) = fold bmerge (map ebtest l) (false,nil)
  | ebtest(PRIMOP(_,vl,wl,ce)) = 
        fold bmerge (map ebtest ce) (false,nil)
(* val (EB,ncand) = fold bmerge (map (ebtest o #3) conts) (false,nil)
 *)

(* Get the combined list of the free variables of all the escaping functions
 * of the FIX, and the labels for each function.
 *)
val (free,escape) =
 let val (f,e) = fold (fn ((v,a,b),(f',e')) =>
		         let val (l,f'') = freevars v
			 in  (merge(f'',f'),(v,l,a,b)::e')
			 end)
                      escape (nil,nil)
 in  (remove(escaping, f),e)
 end

(* Get the combined list of the free variables of all the continuation functions
 * of the FIX, and the labels for each function.
 *)
val (contfree,conts) = 
 let val (f,e) = fold (fn ((v,a,b),(f',e')) =>
		         let val (l,f'') = freevars v
			 in  (merge(f'',f'),(v,l,a,b)::e')
                         end)
                      conts (nil,nil)
 in (remove(contvar,f),e)
 end

val contfree = difference(contfree,escaping)

(* Replace knownfuncs defined in this FIX with their free variables. *)
val contfree : lvar list
	= let val (fns,other) = partition knownlvar contfree
	  in  fold (fn ({v,free,...},b) =>
			if exists (fn w => v=w) fns
			then merge(free,b)
			else b) known other
	  end
 
val (free,flag) : lvar list * bool
	= let val (fns,other) = partition knownlvar free
	  in  fold (fn ({v,free,extrac,...},(b,g)) =>
			if exists (fn w => v=w) fns
			then (merge(free,b),(extrac orelse g))
			else (b,g)) known (other,false)
	  end

(* Add the free variables of knownfuncs in this FIX which were spilled into
 * the closure. 
 *)
val free = merge(collected,free)

(* if escaped functions call those continuations defined in this FIX , merge 
 * the free varaibles together 
 *)
val contlvars = fn v => exists (fn w => v =w) contvar

(* val free : lvar list
        = if (exists contlvars free) orelse flag then merge(contfree,free)
          else free 
val free = difference(free,contvar) *)

val flag = (exists contlvars free) orelse flag
val free = difference(free,contvar)

(* Replace knownfuncs defined elsewhere with their free variables, and 
 * escaping functions defined elsewhere with their closures and the continu-
 * ation variables with their extra arguments .  The function environment 
 * which tells that certain free variables are known functions and gives their 
 * free variables must be kept for applications of the functions in the bodies 
 * of the escaping functions of the FIX. 
 *)
val (free,functionEnv) : lvar list * env (* only need function mapping here *)
	= let val (free,env') = funcAnalysis(free,initEnv)
	      val free = sameClosureOpt(free,initEnv)
	  in  (free,env')
	  end

val (contfree,functionEnvCont) : lvar list * env
	= let val (free,env') = funcAnalysis(contfree,initEnv)
	      val free = sameClosureOpt(free,initEnv)
	  in  (free,env')
	  end

(* It's possible that known functions and escaped continuation functions are 
 * in the same FIX and continuation functions may be just some free variables 
 * of these known functions. Since these circumstances are very rare......, 
 * we don't like those extra arguments of continuations functions also become 
 * the extra args of known functions, thus we will use a single closure 
 * representation for continuation functions.  2/12/91
 *)
val ktag = fold (fn ({v,free,callc,extrac,args,body,env,label},t) => 
                    let val x = length(free)+length(args)+(2 * tag)
                     in (extrac andalso (x >= maxfree)) orelse t
                    end) known false

val known = map (fn {v,free,callc,extrac,args,body,env,label} =>
                  if ktag then {v=v,free=free,callc=callc,extrac=false,args=args,
                                body=body,env=env,label=label}
                  else {v=v,free=free,callc=callc,extrac=extrac,args=args,
                                body=body,env=env,label=label}) known

(* flag = T means we are going to build one closure with all free vars anyway *)
val flag = ktag orelse flag


(* We find an appropriate representation for these free variables based on 
 * the initEnv . 
 *)

(* a temporary version of lookup 
 * fun lookup0(Env [(v,Closure {functions=functions,contents=contents,...})], x)
 *     = in'(x,map #1 contents)
 * | lookup0 _ = error "compiler bugs in closure1.sml --- lookup0"
 *)                    

(* Look through a list of enclosing closures to see if we can use them.
 *    retract : lvar list * ((lvar * (lvar * object)) list) -> lvar list 
 *)
fun retract(fl,cl) =
  let fun weight (v,e) = 
        let fun inImmed x = (lookup(e,x); true) handle Lookup _ => false 
            val t = sublist inImmed fl
         in (length t,t,v)
        end
      val clinfo = sublist (fn (k,_,_) => (k > 0)) (map weight cl)
      val clinfo = sublist (fn (k,_,v) => (k > 3) orelse (in'(v,fl))) clinfo
      val op btr = fn ((k1,_,_),(k2,_,_)) => (k1 > (k2 : int))
      val clinfo = (Sort.sort op btr) clinfo
      fun repclos (fl,nil) = fl
        | repclos (fl,(_,t,v)::tl) = 
             let val t' = sublist (fn x => in'(x,fl)) t
	     in if ((length(t')) > 3) orelse in'(v,fl)
                then repclos(enter(v,difference (fl,t')),tl)
                else repclos(fl,tl)
             end
   in repclos(fl,clinfo)
  end

fun thinner(fl,cl) = (* delete redundant free vars, but not closures *)
  let val clinfo = sublist (fn (v,e) => in'(v,fl)) cl
      fun inImmed x = 
         let fun inImmedlist nil = false
               | inImmedlist ((v,e)::tl) = 
                  (lookup(e,x); (v<>x)) handle Lookup _ => inImmedlist tl
          in inImmedlist clinfo 
         end
   in (sublist (not o inImmed) fl)   
  end

fun preproc(fl,cl) = 
  let val m = length(fl)
      fun inImmedAll (v,e) =
         let fun inImmed x = (lookup(e,x); true) handle Lookup _ => false
          in (m <= length(sublist inImmed fl))
         end
      fun h nil = if method <> 2 then thinner(fl,cl) else retract(fl,cl)
        | h ((v,e)::tl) = if inImmedAll (v,e) then [v] else h(tl)
   in h cl
  end 

fun preproc2(newfl,cl,k) = 
  let fun weight (v,e) = 
        let fun inImmed x = (lookup(e,x); true) handle Lookup _ => false 
            val t = sublist inImmed newfl
         in (length t,t,v)
        end
      val m = length(newfl)
      val clinfo = sublist (fn (k,_,_) => (k>0))  (map weight cl)
      val op btr = fn ((k1,_,_),(k2,_,_)) => (k1 > (k2 : int))
      val clinfo = (Sort.sort op btr) clinfo
   in case clinfo of 
        nil => (newfl,NONE) 
      | _ => (let val (j,t,v) = hd clinfo
                  val doit = ((j > 1) andalso ((m - j) <= k))
                  val doit = doit orelse (j > (m div 2) + 1)
                  val doit = doit andalso (m > k+1)
               in if doit then (difference(newfl,t),SOME v)
                  else (newfl,NONE)
              end )
  end
      
   
(* Try to see if there are any closures already made in the environment which
 * could dramatically decrease the size of current closures.
 *  shorten : lvar list -> lvar list
 *)
fun shorten(fl) = case fl of nil => nil 
                           | _ => (if method <> 2 then fl
                                   else if length(fl) < 4 then fl
                                        else retract(fl,closlist))

(* Given a list of free variables, if k = 0 we use aggressive approach, if 
 * k = tag -1, we use deligent (or lazy) approach. However when EB is false, 
 * we'll use conservative approach anyway.
 *) 

fun calleealloc(fl,k) =
  let val restm = sublist (fn x => in'(x,fl)) (tl(fvl)) (* def: conservative *)
      val rest = if (length restm) = tag then (tl restm)
                 else restm
      exception FAIL of lvar list * int
      fun first(nil,0,res) = res
        | first(nil,k,res) = raise FAIL (res,k)
        | first(_,0,res) = res
        | first(hd::tl,k,res) = first(tl,k-1,res@[hd])
      and first0(wl,vl,k) = first(wl,k,nil) handle FAIL (c,i) => 
                               (first(vl,i,c) handle FAIL (res,_) => res)

      fun punch(nil,nil) = nil
        | punch(nil,tl) = tl 
        | punch(tl,nil) = tl
        | punch(hd1::tl1,hd2::tl2) =hd1::punch(tl1,tl2)

      val (EB,ncand) = fold bmerge (map (ebtest o #4) conts) (false,nil)

      (* the getv function could be modified by doing more data flow analysis *)
      fun getv(vl,k,rest) = (* choose k lvars from vl, default is rest*)
         if length(vl) <= k then punch(vl,rest)
         else if (not EB)                    (* leaf cont nodes *)
              then punch (rest,first0(difference(vl,uniq rest),nil,k))
              else (let val wl = difference(vl,ncand)
                        val ul = difference(vl,wl)
                     in punch (first0(wl,ul,k),rest)
                    end)
   in if k=0                      
      then (preproc(fl,closlist),rest)
      else (let val newfl = preproc(fl,closlist) 
             in if length(newfl) <= 1 then (newfl,rest)
                else case preproc2(newfl,closlist,k) of 
                       (vl,NONE) => 
                         (let val t = length(vl)
                              val cand = if t <= (k+1) 
                                         then tl(punch(vl,restm))
                                         else getv(vl,k,rest)
                           in (difference(vl,uniq cand),cand)
                          end)
                     | (vl,SOME v) => 
                         (if length(vl) <= k then 
                              ([v],getv(vl,k,rest))
                          else 
                            (let val rest0 = if length(rest) = k 
                                             then (tl rest)
					     else rest
                                 val cand = getv(vl,k-1,rest0)
                                 val result0 = difference(vl,uniq cand)
                              in (result0,v::cand)
                             end)
                          )
            end)
  end (* calleealloc *)         

(* Decide which variables go to closures and which variables go to callee-save 
 * registers. The case when both conts and escape (known spills) appears is 
 * simplified for current implementations, we could use "flag" tag to do some 
 * optimizations on this.
 *)

val (x : int,y : int) = (length(conts),length(escape))

val (newfree,rest) = case (conts,escape,collected) 
    of (nil,_,_) =>  (shorten(free),nil)
     | (_,nil,nil) => if flag then calleealloc(contfree,0)
                      else calleealloc(contfree,tag-1)
     | (_,nil,_) => (let val left = difference(contfree,free)
                         val (nfl,rest) = if flag then calleealloc(left,0)
                                           else calleealloc(left,tag-1)
                      in (merge(free,nfl),rest)
                     end)
     | _ => (let val fl = merge(free,contfree)
              in calleealloc(fl,0)     (* this is simpified, could be expanded *)
             end) 

(* Given the functions to be defined in the closure (escape), the free
   variables which should be contained in the closure (free), and their
   current locations (env), decide on a closure representation. *)
val closure = closureStrategy(escape,newfree,initEnv)
val cname = case escape of [] => closureLvar()
                         | ((v,_,_,_)::_) => v

val (noclos,cname,closure) = 
     case (escape,newfree)
       of (nil,nil) => (true,cname,closure)
        | (nil,[w]) => (case lookup(initEnv,w) of 
                           (Function _,_) => error "cps/closure437"
                         | (obj,_) => (true,w,obj))
        | _ => (false,cname,closure)

fun fillin(t,(VAR x,obj)::tl) = (VAR x,obj)::(fillin(t,tl))
  | fillin(t,(INT 0,Value)::tl) = t::tl
  | fillin(t,nil) = error "errors in closure1.sml --- fillin"

val rest = if noclos then newfree@rest else rest
val rest = let val wl = tl fvl
               val left = sublist (fn x => not (in'(x,uniq wl))) rest
               val vl = uniq rest
               val base = map (fn x => if in'(x,vl) then SOME x else NONE) wl
               fun h((SOME x)::tl,ul) = (SOME x)::h(tl,ul)
                 | h(NONE::tl,u::ul) = (SOME u)::h(tl,ul)
                 | h(NONE::tl,nil) = NONE::h(tl,nil)
                 | h(nil,nil) = nil
                 | h _ = error "errors in closure1.sml --- rest/h"
            in rev(h(rev base,left))
           end 

fun getenv (SOME x) = (case lookup(initEnv,x) of 
                           (Function _,_) => error "cps/closure437"
                         | (obj,_) => (VAR x,obj))
  | getenv NONE = (INT 0,Value)

val extramap = if noclos 
               then map getenv rest
               else fillin((VAR cname,closure),(map getenv rest))
   
val _ = app (fn (v,l,a,b) => addinc(v,l,extramap)) conts

val cl = uniq (clean (map #1 extramap))
val extraenv = fold (fn ((VAR x,c),l) => (x,c)::l
                      |  (_,l) => (mkLvar(),Value)::l) extramap nil

fun mkFnMap c : (lvar * object) list
	= map (fn{v,free,callc,extrac,label,...} =>
                if extrac 
                then (v,Function{label=label,free=merge(cl,free)})
         	else if callc then (v,Function{label=label,free=enter(c,free)})
		     else (v,Function{label=label,free=free}))
	      known

(* Final construction of the environment for each standard function. *)
val closureFrags : (lvar * lvar list * cexp * env * lvar list) list
	= case escape of [] => []
	| ((v,_,_,_)::_) =>
	  let val env = fold augment (mkFnMap v) functionEnv
	      fun f ((v,l,args,body),i) =
		let val cname = closureLvar()
                    val k = nth(args,1) handle Nth => 
                         (error "escaped user functions has less than 2 arguments")
                    val kl = extraLvar(tag)
                    val _ = addoutc(k,map VAR kl)
		    val env = fold (fn (v,b) => augment((v,Value),b))
				(args@kl@contvar) (offset(i,closure,cname,env))
(***> 
		    val _ = COMMENT(fn () => (print "\nEnvironment in escaping ";
					      vp v; print ":\n";
					      printEnv env))
<***)
		in  (l,cname::(args@kl),body,env,k::kl)
		end
	  in  formap f escape
	  end

(* Final construction of the environment for each known function. *)
val fnMap = mkFnMap cname

val registerFrags : (lvar * lvar list * cexp * env * lvar list) list
	= map (fn{v,free,callc,extrac,args,body,env=env',label} =>
		let val vl = sublist iscont args

                    val _ = if length(vl)>1 
                            then error "known funs have > 1 cont args " 
                            else ()

                    val extra = fold (fn (k,l) => (let val kl = extraLvar(tag)
                                                       val _ = addoutc(k,map VAR kl)
                                                    in kl@l
                                                   end)) vl nil
                    val env =
		      fold (fn (v,env') =>
				case lookup(initEnv,v)
				  of (Function _,_) => error "cps/closure.223"
				   | (obj,_) => augment((v,obj),env'))
			   free
			   (fold (fn (v,b) => augment((v,Value),b))
				 (args@extra)
				 (fold augment fnMap
				  (if extrac then (inc System.Control.CG.knowncl;
						   fold augment extraenv env')
                                   else if callc
				   then (inc System.Control.CG.knowncl;
				         augment((cname,closure),env'))
			  	   else env')))
                    val env = fold (fn (x,env') => augment((x,Value),env')) 
                                  contvar env

(***>               val _ = COMMENT(fn () => (print "\nEnvironment in known ";
					      vp v; print ":\n";
					      printEnv env))
<***)

                    val free =  (if extrac then merge(cl,free) 
                                           else if callc then enter(cname,free)
                                                else free)
		    val args = args @ free @ extra
		in  if vl = nil then (label,args,body,env,fvl)
                    else (label,args,body,env,vl@extra)
		end)
	      known

val contFrags : (lvar * lvar list * cexp * env * lvar list) list
       = case conts of [] => []
       | ((v,_,_,_)::_) => 
	     (let val env = fold augment fnMap functionEnvCont
                  val env = fold (fn (x,env') => augment((x,Value),env')) 
                                  contvar env
                  val env = fold augment extraenv env

(***>             val _ = COMMENT(fn () => (print "\nEnvironment in continua.. ";
		        		      vp v; print ":\n";
					      printEnv env))
<***)

		  fun f (v,l,args,body) = 
                     let val env = fold (fn (v,b) => augment((v,Value),b))
                                          args env
                         val args' = args@(map #1 extraenv)
                      in (l,args',body,env,v::(map #1 extraenv))
                     end

               in map f conts
              end)
		    

val contents = if noclos then nil
               else (let val Closure{functions,contents,...} = closure
	              in  map (LABEL o #2) functions @ map (VAR o #1) contents
	             end)


(* Add profiling code if flag is on. *)
fun mkrexp(contents,cname) = if noclos then fn x => x
                             else mkrexp'(contents,cname)
and mkrexp'(contents,cname) = 
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
      of [] => (fn ce => ce,registerFrags@contFrags,
                  fold augment (fnMap@(map (fn x => (x,Value)) contvar)) initEnv)
       | _ =>
	  let val frags = closureFrags@registerFrags@contFrags
	      val env = fold (fn(a,b) => augment((#1 a,Value),b))
		             closureFrags initEnv
	      val (contents,header) = 
		if noclos then (nil,fn x => x) 
                else recordEl(map (fn v => (v, OFFp0)) contents, env)
	      val env = fold augment fnMap env
              val env = fold augment (map (fn x => (x,Value)) contvar) env
	      val env = if noclos then env else augment((cname,closure),env)
	  in
(***>        COMMENT(fn () => (print "\nEnvironment after FIX:\n";
	                        printEnv env; pr "MAKENV DONE.\n\n"));
<***)
	      (header o mkrexp(contents,cname),frags,env)
	  end
end

val c0 = nth(vl,1) handle Nth => error "outermost fun has less than 2 args 4973"
val cl = extraLvar(tag)
val vl' = (vl@cl) before addoutc(c0,(map VAR cl))
val env1 = fold (fn(v,b) => augment((v,Value),b)) (f::(vl')) (Env [])
fun close(ce,env,fvl) =
  case ce
    of FIX(bindings,b) =>
       (let val (header,frags,env') = makenv(env,bindings,fvl)
	in  FIX(map (fn(v,args,a,env,fv') =>
				(v,args,close(a,env,fv'))) frags,
		header(close(b,env',fvl)))
	end handle Lookup(i,e) => (print "LOOKUP FAILS in close(FIX";
				   ilist (map #1 bindings);
				   print ") on "; vp i;
				   print "\nin environment:\n";
                                   printEnv e;
				   APP(VAR 0,nil)))
     | APP(f,args) =>
       (let val(obj,_) = clookup(env,f)
            fun mlabel x = if iscontv x then (#1 (findextra x)) else x
	in  case obj
	      of Closure{functions,offset,...} =>
		   let val c0 = nth(args,1) handle Nth => 
                                 error "esc user fun has less than 2 args 7988"
                       val (c0',extra) = findextra c0
                       val args' = args@extra
                       val (_,header) = fixAccess(f::args',env)
		       val (_,label) = nth(functions,offset)
		       val call = APP(LABEL label,f::(map mlabel args'))
		   in  if !CGoptions.profile
		       then header(prof(STDKCALLS,1,call))
		       else header call
		   end
	       | Function{label,free} =>
		   let val cl0 = sublist iscontv args
                       fun h(x,l) = let val (_,nl) = findextra x
                                     in (nl@l)
                                    end
                       val extra = (fold h cl0) nil
                       val args' = (args@(map VAR free))@extra
		       val (_,header) = fixAccess(args',env)
		       val call = APP(LABEL label,(map mlabel args'))
		   in  if !CGoptions.profile
		       then header(prof(KNOWNCALLS,1,call))
		       else header call
		   end
	       | Value =>
		   let val n = length(args)
                       val c0 = if n = 1 then f else (nth(args,1) handle Nth =>
                                error "esc user fun has less than 2 args 7989")
                       val (label,extra) = findextra c0
                       val args' = args@extra
		       val (_,header) = fixAccess(f::args',env)
		       val call = if n = 1 then APP(label,args')
                                  else (let val l = mkLvar()
                                         in SELECT(0,f,l,APP(VAR l,
                                                        f::(map mlabel args')))
                                        end)
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
	in  header (SWITCH(v,map (fn c => close(c,env',fvl)) l))
	end
     | RECORD(l,v,c) =>
	let val (l,header) = recordEl(l,env)
	    val ce = close(c,augment((v,Value),env),fvl)
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
	in  header(SELECT(i,v,w,close(c,augment((w,select(i,obj)),env),fvl)))
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
	in  header (PRIMOP(i,args,rets,map (fn c => close(c,env,fvl)) l))
	end
in  (mkLvar(),f::vl',unrebind(close(ce,env1,c0::cl)))
end

end (* structure Closure *)
