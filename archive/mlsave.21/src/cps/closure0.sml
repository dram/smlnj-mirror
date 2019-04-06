signature CLOSURE =
  sig
    val closeCPS : CPS.function * (CPS.lvar -> bool) * (CPS.lvar -> bool)
				 * (int * int * CPS.cexp -> CPS.cexp) ->
			CPS.function * (CPS.lvar -> bool) * (CPS.lvar -> bool)
  end

functor Closure(val maxfree : int) : CLOSURE =
struct

open CPS FreeMap Access Profile SortedList
fun partition f l = fold (fn (e,(a,b)) => if f e then (e::a,b) else (a,e::b))
			 l (nil,nil)
fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl nil = nil
  in  subl
  end
local val save = (!saveLvarNames before saveLvarNames := true)
      val closure = namedLvar(Symbol.new "closure")
in    val closureLvar = (saveLvarNames := save; fn () => dupLvar closure)
end

datatype closure = CLOSURE of {functions : (lvar * lvar) list,
			       values : lvar list,
			       closures : (lvar * closure) list,
			       offset : int,
			       stamp : lvar}
datatype function = RegisterFn of {label:lvar,free:lvar list}
		  | ClosureFn of {label : lvar,
				  closurename : lvar,
				  closure : closure}
datatype object = Function of function
		| Closure of closure
		| Value
datatype access = Direct
		| Path of (lvar * closure * accesspath)
datatype env = Env of {valueMap : lvar list,
		       functionMap : (lvar * function) list,
		       closureMap : (lvar * closure) list}
fun mkEnv(valueMap,functionMap,closureMap) =
	Env{valueMap=valueMap,functionMap=functionMap,closureMap=closureMap}
fun mkClosure(functions,values,closures) =
     CLOSURE{functions=functions,values=values,closures=closures,
	     offset=0,stamp=mkLvar()}
val env0 = mkEnv(nil,nil,nil) (* The empty environment *)
fun addValue(m,Env{valueMap,functionMap,closureMap}) =
	mkEnv(m::valueMap,functionMap,closureMap)
fun addClosure(m,Env{valueMap,functionMap,closureMap}) =
	mkEnv(valueMap,functionMap,m::closureMap)
fun addFunction(m,Env{valueMap,functionMap,closureMap}) =
	mkEnv(valueMap,m::functionMap,closureMap)
fun addtoEnv(v,Value,env) = addValue(v,env)
  | addtoEnv(v,Closure cl,env) = addClosure((v,cl),env)
  | addtoEnv(v,Function f,env) = addFunction((v,f),env)
fun combineEnv(Env{valueMap=v0,functionMap=f0,closureMap=c0},
	       Env{valueMap=v1,functionMap=f1,closureMap=c1}) =
	mkEnv(v0@v1,f0@f1,c0@c1)

val vp = (String.print o Access.lvarName)
val pr = output std_out
fun plist p l = (app (fn v => (pr " "; p v)) l; pr "\n"; ())
val ilist = plist vp
fun printEnv(Env{valueMap,functionMap,closureMap}) =
  let val seen : lvar list ref = ref nil
      val tlist = plist (fn (a,b) => (vp a; pr "/L"; Integer.print b))
      val flist = plist (fn (a,RegisterFn{label,...}) => (vp a;
							  pr "/k";
							  Integer.print label)
			  | (a,ClosureFn{label,...}) =>  (vp a;
							  pr "/e";
							  Integer.print label))
      fun v nil = () | v l = (pr "Vals:"; ilist l)
      fun f nil = ()
	| f l = (pr "Funs:"; flist l)
      fun c(pr,(v:int,CLOSURE{functions,values,closures,offset,stamp})::m) =
	let fun see v = seen := merge([v],!seen)
	    fun saw v = exists (fn w => v=w) (!seen)
	in  pr(); print "Closure "; vp v; print "/"; print stamp;
	    print " @"; print offset;
	    if saw stamp then (print "(seen)\n"; ())
	    else (see stamp; print ":\n";
		  case functions of nil => ()
		     | _ => (pr(); print "  Funs:"; tlist functions; ());
		  case values of nil => ()
		     | _ => (pr(); print "  Vals:"; ilist values);
		  case closures of nil => ()
		     | _ => c(fn () => (pr(); print "  "),closures));
	    c(pr,m)
	end
	| c(_,nil) = ()
  in  v valueMap; f functionMap; c(fn()=>"",closureMap)
  end

(* "Alpha conversion": the closure converter introduces duplicate bindings
   at function arguments (the free variables of known functions) and at
   SELECT's and OFFSET's from closures.  This function restores unique
   bindings, and also eliminates OFFSET's of 0 (which are introduced as
   a side effect of trying to improve lazy display).  It assumes that a
   FIX has no free variables. *)
fun unrebind ce =
let fun rename rebind v =
      let fun f nil = v
	    | f ((w:int,v')::t) = if v=w then v' else f t
      in  f rebind
      end
    fun f (l,args,b) =
      let val (args',rebind') = fold (fn(v,(args',rebind')) =>
					let val v' = dupLvar v
					in  (v'::args',(v,v')::rebind')
					end)
				     args (nil,nil)
      in  (l,args',g(b,rebind'))
      end
    and g(ce,rebind) =
      let val rename = rename rebind
	  val rec h =
	       fn RECORD(vl,w,e) =>
		    RECORD(map (fn(v,p) => (rename v,p)) vl,w,h e)
		| OFFSET(0,v,w,e) => g(e,(w,rename v)::rebind)
		| OFFSET(i,v,w,e) =>
			let val w' = dupLvar w
			in  OFFSET(i,rename v,w',g(e,(w,w')::rebind))
			end
		| SELECT(i,v,w,e as APP(x,args)) =>
			let val w' = dupLvar w
			in  if w=x
			    then SELECT(i,rename v,w',APP(w',map rename args))
			    else SELECT(i,rename v,w',g(e,(w,w')::rebind))
			end
		| SELECT(i,v,w,e) =>
			let val w' = dupLvar w
			in  SELECT(i,rename v,w',g(e,(w,w')::rebind))
			end
		| APP(f,vl) => APP(f,map rename vl)
			(* HACK: f is always a label or from a SELECT, so
			   we never need rename. *)
		| FIX(l,e) => FIX(map f l,h e)
		| SWITCH(v,el) => SWITCH(rename v,map h el)
		| PRIMOP(i,vl,wl,el) => PRIMOP(i,map rename vl,wl,map h el)
      in  h ce
      end
in  g(ce,nil)
end
(* TEMPORARY DEBUGGING STUFF *)
val alphac = CPSoption.alphac
val comment = CPSoption.comment
val unrebind = fn x => if !alphac then unrebind x else x
fun COMMENT f = if !comment then (f(); ()) else ()

fun formap f =
  let fun iter(nil,_) = nil
	| iter(hd::tl,i) = f(hd,i)::iter(tl,i+1)
  in  iter o (fn l => (l,0))
  end

exception Lookup
fun lookup0 constant =
  fn (env as Env{valueMap,functionMap,closureMap}) =>
    fn looking =>
      let (* Closures may be duplicated in the 'tree'; don't look at them twice. *)
	  val seen : lvar list ref = ref nil
	  fun see v = seen := merge([v],!seen)
	  fun saw v = exists (fn w => v=w) (!seen)
	  (* Breadth-first-search of the closure tree. *)
	  fun bfs(((cv,cl),p)::m) =
	    let val CLOSURE{functions,
			    values,
			    closures,
			    offset,
			    stamp} = cl
		fun element i =
		 let val p' = i-offset
		 in  if p'<0
		     then (print "\nNegSel looking for ";
			   print(Access.lvarName looking); print " in\n";
			   printEnv env)
		     else ();
		     p'::p
		 end
		fun cls(nil,i) =
		     let val off = i - length closures
		     in  bfs(m @ formap (fn(cm,j) => (cm,element(off+j))) closures)
		     end
		  | cls((v,c)::t,i) =
			if looking=v then (element i,0,Closure c)
			else cls(t,i+1)
		fun vls(nil,i) = cls(closures,i)
		  | vls(v::t,i) =
			if looking=v then (element i,0,Value)
			else vls(t,i+1)
		fun fns(nil,i) = vls(values,i)
		  | fns((v,l)::t,i) =
			if looking=v
			then (p,i-offset,Function(ClosureFn{label=l,closure=cl,
							    closurename=cv}))
			else fns(t,i+1)
	    in  if saw stamp then bfs m
		else (see stamp; fns(functions,0))
	    end
	    | bfs nil = raise Lookup
	  (* Before the bfs, look at the flat environment. *)
	  fun l0 nil =
	       let val (p,off,r) = bfs (formap (fn(cm,i) => (cm,[i])) closureMap)
		   val (n::t) = rev p
		   fun f nil = OFFp off | f(h::t) = SELp(h,f t)
		   val (v,c) = nth(closureMap,n)
	       in  (r,Path(v,c,f t))
	       end
	    | l0 ((v,c)::tl) = if looking=v then (Closure c,Direct) else l0 tl
	  fun l1 nil = l0 closureMap
	    | l1 ((v,f)::tl) = if looking=v then (Function f,Direct) else l1 tl
	  fun l2 nil = l1 functionMap
	    | l2 (v::tl) = if looking=v then (Value,Direct) else l2 tl
      in  if constant looking then (Value,Direct)
	  else l2 valueMap
	  handle Lookup => (print "**LOOKUP: Can't find "; print looking;
			    print " in environment:\n";
			    printEnv env;
			    raise Lookup)
      end

fun select(i,CLOSURE{functions,values,closures,offset,stamp}) =
	let val index = offset + i - length functions - length values
	in  if index < 0 then Value
	    else Closure((#2 o nth)(closures,index))
	end
fun offset(i,CLOSURE{functions,values,closures,offset,stamp},v,env) =
  addClosure((v,CLOSURE{functions=functions,values=values,closures=closures,
			offset=offset+i,stamp=stamp}),env)

fun closureStrategy(bindings,free,lookup) = (* temporary *)
  let val (values,closures) = fold (fn(a,(v,c)) =>
				     let val (obj,_) = lookup a
handle Lookup => (print "LOOKUP FAILS in closureStrategy\n"; (Value,Direct))
				     in  case obj
					   of Closure cl => (v,(a,cl)::c)
					    | _ => (a::v,c)
				     end)
				   free (nil,nil)
  in  mkClosure(map (fn(v,l,_,_) => (v,l)) bindings,values,closures)
  end
type info = {v:lvar,fns:lvar list,other:lvar list,args:lvar list,
	  body:cexp,label:lvar,env:env,callc:bool}
(* merge the free variables of recursive register functions *)
fun regf bindings =
let fun pack m =
	let fun getother w =
		let fun g(({v,...}:info,other,_)::tl) = if v=w then other
			else g tl
                      | g nil = ErrorMsg.impossible "nil 4849 in cps/closure"
		in  g m
		end
	    fun getcallc w =
		let fun g(({v,...}:info,_,callc)::tl) = if v=w then callc
			else g tl
                      | g nil = ErrorMsg.impossible "nil 4848 in cps/closure"
		in  g m
		end
	    fun f (x as {args,fns,...}:info, other, callc) =
		  (x,
		   fold (fn (w,l) => merge(getother w, l)) fns other,
		   callc orelse length args + length other >= maxfree
				orelse exists getcallc fns)
	    val m' = map f m
	 in if exists (fn ({callc,...}:info,_,callc') => callc <> callc') m'
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
			 free=nil,callc=callc}::b,merge(other,f))
			else
			({v=v,args=args,body=body,label=label,env=env,
			 free=other,callc=callc}::b,f))
		     m' (nil,nil)
	 end
 in pack (map (fn (x as {other,callc,...}) => (x,other,callc)) bindings)
end

(* Look up the free variables of a fragment and find out what kind of objects
   they are.  If a subset of free variables are functions from a closure, they
   are replaced by the closure, since one pointer to the closure is enough
   to access anything in it.  This means that even if the closure is needed for
   only one function, a pointer might have to be offset at every application
   of that function.  There doesn't seem to be a right or wrong; the offset
   must be performed somewhere, and if there are two functions, it is impossible
   to determine which to favor.  Another alternative would be to include a
   pointer for each function.  This has implications in closure strategy.
   Register functions are replaced by their free variables, and the complete
   registerFn mapping must be returned.  None of the other free variables
   are mapped. *)
fun funcAnalysis(free,lookup) =
  let fun f(v,(l,env)) =
	let val(obj,_) = lookup v
handle Lookup => (print "LOOKUP FAILS in funcAnalysis\n"; (Value,Direct))
	in  case obj
	      of Value => (merge([v],l),env)
	       | Closure c => (merge([v],l),env)
	       | Function(f as RegisterFn{free,...}) =>
			(merge(free,l),addFunction((v,f),env))
	       | Function(ClosureFn{closure,closurename,...}) =>
			(merge([closurename],l),env)
	end
  in  fold f free (nil,env0)
  end
(* Register functions need to know which of their free variables are
   closures; others are mapped to values. *)
fun closureMap(free,lookup) =
  let fun f nil = env0
	| f (v::tl) =
	    let val env = f tl
		val (what,_) = lookup v
handle Lookup => (print "LOOKUP FAILS in closureMap\n"; (Value,Direct))
	    in  case what
		  of Closure c => addClosure((v,c),env)
		   | _ => addValue(v,env)
	    end
  in  f free
  end

fun fixAccess(args,env,lookup) =
let
fun access(rootvar,(env,header)) =
  let val rec follow =
	fn (v,cl,env,OFFp off,h) =>
		  (offset(off,cl,rootvar,env),
		   h o (fn ce => OFFSET(off,v,rootvar,ce)))
	 | (v,cl,env,SELp(i,OFFp 0),h) =>
		  (addtoEnv(rootvar,select(i,cl),env),
		   h o (fn ce => SELECT(i,v,rootvar,ce)))
	 | (v,cl,env,SELp(i,p),h) =>
		  let val w = mkLvar()
		      val cl' as Closure c = select(i,cl)
		      val env' = addtoEnv(w,cl',env)
		  in  follow(w,c,env',p,h o (fn ce => SELECT(i,v,w,ce)))
		  end
      val (obj,acc) = lookup env rootvar
handle Lookup => (print "LOOKUP FAILS in fixAccess\n"; (Value,Direct))
  in  case acc
	of Direct => (env,header)
	 | Path(start,cl,path) => follow(start,cl,env,path,header)
  end
in  fold access args (env,fn x => x)
end

fun recordEl(l,env,lookup) =
let fun f((v,p),(paths,env,header)) =
      case (lookup env v
handle Lookup => (print "LOOKUP FAILS in recordEl\n"; (Value,Direct)))
	of (_,Direct) => ((v,p)::paths,env,header)
	 | (_,Path(start,_,path)) =>
		((start,combinepaths(path,p))::paths,env,header)
in  fold f l (nil,env,fn x => x)
end

fun makenv0 (freevars,lookup,escapes,markknown,markunknown,prof)
	    (env,bindings: (lvar * lvar list * cexp) list) =
let
val _ = COMMENT(fn() => pr "Beginning makenv...\n")

(* A debugging version of freevars *)
val freevars =
  (fn v => let val free = freevars v
	   in  COMMENT(fn() => (pr "Free in "; vp v; pr ":"; ilist free));
	       free
	   end)

(* Separate functions into those that escape and those which are knownfuncs *)
val (escape,known) = partition (escapes o #1) bindings

(* Label all the escaping functions of the FIX. *)
val escape : (lvar * lvar * lvar list * cexp) list
	= map (fn(v,args,body) => (v,dupLvar v,args,body)) escape

val _ = COMMENT(fn() => pr "Knownfuncs...\n")
(* Mark each known function of the FIX with its free variables. *)
val known
	= map (fn(v,args,body) => {v=v,free=freevars v,args=args,body=body}) known

(* For each known function of the FIX, remove any escaping functions of the
   FIX from its free list and mark that the function requires the closure. *)
val known
	= let fun closureFn v = exists ((fn w => v=w) o #1) escape
	      fun f{v,free,args,body} =
		{v=v,free=sublist (not o closureFn) free,
		 callc=exists closureFn free,
		 args=args,body=body}
	  in  map f known
	  end

(* Separate known functions defined in this FIX from other free variables. *)
val known
	= map (fn {v,free,callc,args,body} =>
		let fun binding w = exists (fn {v,...} => v=w) known
		    val (fns,other) = partition binding free
    		in  {v=v,fns=fns,other=other,callc=callc,args=args,body=body}
		end)
	      known

(* Replace knownfuncs defined in other FIX'es by their free variables, and
   escaping functions defined in other FIX'es by their closures.  Label
   each knownfunc. *)
val known
	= map (fn{v,fns,other,callc,args,body} =>
		let val (other,env) = funcAnalysis(other,lookup env)
		in  {v=v,fns=fns,other=other,callc=callc,args=args,body=body,
		     env=env,label=dupLvar v}
		end)
	      known

(* Merge free variables of knownfuncs that call each other. *)
(* Look at the number of free variables and arguments to each known function
   to be defined.  The cps converter ensures that there are enough registers
   to hold the arguments and leaves one register free for the free variables,
   if any.  Therefore some free variables may have to be spilled into the closure,
   and these must be collected. *)
val (known,collected)
	= regf known

val _ = COMMENT(fn() => pr "Escaping functions...\n")
(* Get the combined list of the free variables of all the escaping functions
   of the FIX. *)
val free : lvar list
	= remove(uniq(map #1 escape),
	         fold merge (map (freevars o #1) escape) nil)

(* Replace knownfuncs defined in this FIX with their free variables. *)
val free : lvar list
	= let val (fns,other) = partition
					(fn w => exists (fn{v,...} => w = v) known)
					free
	      val knownusedmap = sublist
				    (fn {v,...} => exists (fn w => v=w) fns)
				    known
	  in  fold (merge o (fn({free,...},b) => (free,b))) knownusedmap other
	  end

val free = merge(collected,free)

(* Replace knownfuncs defined elsewhere with their free variables, and escaping
   functions defined elsewhere with their closures.  The function environment
   which tells that certain free variables are known functions and gives their
   free variables must be kept for applications of the functions in the bodies
   of the escaping functions of the FIX. *)
val (free,functionEnv) : lvar list * env (* only need function mapping here *)
	= funcAnalysis(free,lookup env)
(* Given the functions to be defined in the closure (escape), the free variables
   which should be contained in the closure (free), and their current locations
   (lookup env), decide on a closure representation. *)
val closure = closureStrategy(escape,free,lookup env)
fun mkFnMap c : (lvar * function) list
	= map (fn{v,free,callc,label,...} =>
(* CAUTION: should be merge(free,[c]); works because free is always nil. *)
		if callc then (v,RegisterFn{label=label,free=free@[c]})
		else (v,RegisterFn{label=label,free=free}))
	      known
val closureFrags : (lvar * lvar list * cexp * env) list
	= case escape of nil => nil
	| ((v,_,_,_)::_) =>
	  let val fnMap = mkFnMap v
	      val env = fold addFunction fnMap functionEnv
	      fun f ((v,l,args,body),i) =
		let val cname = closureLvar()
		    val env = fold addValue args (offset(i,closure,cname,env))
		    val _ = COMMENT(fn () => (print "\nEnvironment at escaping ";
					      Integer.print v; print ":\n";
					      printEnv env))
		in  markunknown l; (l,cname::args,body,env)
		end
	  in  formap f escape
	  end
val cname = closureLvar()
val fnMap = mkFnMap cname
val registerFrags : (lvar * lvar list * cexp * env) list
	= map (fn{v,free,callc,args,body,env=env',label} =>
		let val env = closureMap(free,lookup env)
		    val env = fold addValue args
				   (fold addFunction fnMap env)
		    val env = combineEnv(env,env')
		    val (env,free) =
			  if callc
			  then (addClosure((cname,closure),env),
				free @ [cname])
			  else (env,free)
		    val _ = COMMENT(fn () => (print "\nEnvironment at known ";
					      Integer.print v; print ":\n";
					      printEnv env))
		    val args = args @ free
		in  markknown label; (label,args,body,env)
		end)
	      known
val contents = let val CLOSURE{functions,values,closures,...} = closure
	       in  map #2 functions @ values @ map #1 closures
	       end
in  case contents
      of nil => (fn ce => ce,registerFrags,fold addFunction fnMap env)
       | _ =>
	  let val frags = closureFrags@registerFrags
	      val env = fold (fn(a,b) => addValue(#1 a,b)) closureFrags env
	      val (contents,env,header) = recordEl(recordpath contents,env,lookup)
	      val len = length contents
	      val rexp =
		if not(!CGoptions.profile) then fn ce => RECORD(contents,cname,ce)
		else if len < CLOSURESLOTS then
		  fn ce => prof(CLOSURES+len,1,RECORD(contents,cname,ce))
		else fn ce => prof(CLOSURES,1,
			      prof(CLOSUREOVFL,len,RECORD(contents,cname,ce)))
	      val header = header o rexp
	      val env = fold addFunction fnMap env
	      val env = addClosure((cname,closure),env)
	      val _ = COMMENT(fn () => (print "\nEnvironment after FIX:\n";
					printEnv env))
	  in  (header,frags,env)
	  end
     before COMMENT(fn() => pr "makenv done.\n")
end

fun closeCPS((f,vl,ce),constant,escapes,prof) =
let
val lookup = lookup0 constant
val unknownset = Intset.new()
val knownset = Intset.new()
val markknown = Intset.add knownset
val markunknown = Intset.add unknownset
val makenv = makenv0(freemapClose(ce,constant),lookup,escapes,
		     markknown,markunknown,prof)
val env1 = fold addValue (f::vl) env0
fun close(ce,env) =
  case ce
    of FIX(bindings,b) =>
	let val (header,frags,env') = makenv(env,bindings)
	in  FIX(map (fn(v,args,a,env) =>
				(v,args,close(a,env))) frags,
		header(close(b,env')))
	end
     | APP(f,args) =>
	let val(obj,_) = lookup env f
handle Lookup => (print "LOOKUP FAILS in close(APP)\n"; (Value,Direct))
	in  case obj
	      of Function(ClosureFn{label,closurename,...}) =>
		   let val (_,header) = fixAccess(f::args,env,lookup)
		       val call = APP(label,f::args)
		   in  if !CGoptions.profile
		       then header(prof(STDKCALLS,1,call))
		       else header call
		   end
	       | Function(RegisterFn{label,free}) =>
		   let val args' = args@free
		       val (_,header) = fixAccess(args',env,lookup)
		       val call = APP(label,args')
		   in  if !CGoptions.profile
		       then header(prof(KNOWNCALLS,1,call))
		       else header call
		   end
	       | _ =>
		   let val l = mkLvar()
		       val (_,header) = fixAccess(f::args,env,lookup)
		       val call = SELECT(0,f,l,APP(l,f::args))
		   in  if !CGoptions.profile
		       then case args
			      of [_] => header(prof(CNTCALLS,1,call))
			       | _ => header(prof(STDCALLS,1,call))
		       else header call
		   end
	end
     | SWITCH(v,l) =>
	let val (env',header) = fixAccess([v],env,lookup)
	in  header (SWITCH(v,map (fn c => close(c,env')) l))
	end
     | RECORD(l,v,c) =>
	let val (l,env,header) = recordEl(l,env,lookup)
	    val ce = close(c,addValue(v,env))
	    val len = length l
	    val rexp = if not(!CGoptions.profile) then RECORD(l,v,ce)
			else if len < RECORDSLOTS
			 then prof(RECORDS+len,1,RECORD(l,v,ce))
			else prof(RECORDS,1,prof(RECORDOVFL,len,RECORD(l,v,ce)))
	in  header rexp
	end
     | SELECT(i,v,w,c) =>
	let val (env,header) = fixAccess([v],env,lookup)
	    val (obj,_) = lookup env v
handle Lookup => (print "LOOKUP FAILS in close(SELECT)\n"; (Value,Direct))
	in  header(SELECT(i,v,w,
			  case obj
			    of Value => close(c,addValue(w,env))
			     | Closure cl =>
				close(c,addtoEnv(w,select(i,cl),env))
			     | _ => ErrorMsg.impossible "uncool #1"))
	end
     | OFFSET(i,v,w,c) => ErrorMsg.impossible "OFFSET in cps/closure.sml!"
     | PRIMOP(i,args,rets,l) =>
	let val (env',header) = fixAccess(args,env,lookup)
	    val env'' = fold addValue rets env'
	in  header (PRIMOP(i,args,rets,map (fn c => close(c,env'')) l))
	end
in  ((mkLvar(),f::vl,unrebind(close(ce,env1))),
     Intset.mem knownset,Intset.mem unknownset)
end

end (* structure Closure *)

