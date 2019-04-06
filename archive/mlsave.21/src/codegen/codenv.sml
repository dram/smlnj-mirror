(* codenv.sml *)

signature CODENV =
  sig
    structure L : LAMBDA sharing L = Lambda
    structure A : ACCESS sharing A = Access
    type Env
    type Label
    datatype Access = KNOWN of Label
		    | PATH of int list
		    | CONST of int
    val env0 : Env
    val lookup : Env -> A.lvar -> Access
    val augment : Env * (A.lvar * Access) -> Env
    val codenv : L.lexp ->
		 Env * (L.lexp * Env * int ->
			Env * (Label * L.lexp * Env) list * Access list)
  end

functor Codenv(Labels : sig type Label
			    val newlabel : unit -> Label
			end) : CODENV =
    
struct

structure L : LAMBDA = Lambda
structure A : ACCESS = Access

open Basics Labels L PrintUtil

type lvar = A.lvar

datatype Access = KNOWN of Label
		| PATH of int list
		| CONST of int

(* A closure contains functions (for which it is created), values which
   are the free variables of the functions, and links to other closures.
   There is a special case where the functions list is nil; this happens
   for unnamed functions (APP(FN _,FN _) and FIX both produce named functions,
   but a solitary FN is unnamed).  In this case, a single, unnamed function is
   implicit. *)
datatype closure = CLOSURE of {functions : lvar list,
			       values : lvar list,
			       closures : closure list,
			       stamp : int}

datatype Env = ENV of {values : (lvar * Access) list,
		       closures : (closure * Access) list}

(* The standard empty environment. *)
val env0 = ENV{values=nil,closures=nil}

(* Print a closure for debugging purposes. *)
fun prClosure(CLOSURE{functions,values,closures,stamp}) =
  let val pr = output std_out
      val size = length values + length closures + max(length functions,1)
  in  pr "Closure "; print stamp; pr ", size "; print size; newline();
      pr " Functions:";
      case functions of nil => pr " <unnamed>"
         | _ => app (fn v => (pr " "; pr (A.lvarName v))) functions;
      newline();
      case values of nil => ()
         | _ => (pr " Values:";
		 app (fn v => (pr " "; pr (A.lvarName v))) values;
		 newline());
      case closures of nil => ()
         | _ => (pr " Closures:";
		 app (fn CLOSURE{stamp,...} => (pr " "; print stamp)) closures;
		 newline())
  end

(* Make a uniquely stamped closure. *)
local val count = ref 0 in
fun mkClosure(functions,values,closures) =
	let val stamp = !count
	    val closure = CLOSURE{functions=functions, values=values,
				  closures=closures, stamp=stamp}
	in  inc count;
	    if !CGoptions.closureprint then prClosure closure else ();
	    closure
	end
end

exception Root
fun root [r] = r
  | root (_::tl) = root tl
  | root nil = raise Root

fun sublist test =
    let fun subl(a::r) = if test a then a::(subl r) else subl r
          | subl nil = nil
    in  subl
    end

(* The difference function for two sets of lvars. *)
fun diff (a : lvar list,b : lvar list) =
    let fun z nil = nil
	  | z (v::tl) =
		let val more = z tl
		in  if exists (fn w => v=w) b
		    then more
		    else v::more
		end
    in z a
    end

(* Add a value to the current scope. *)
fun augment(ENV{values,closures}, v) =
	ENV{values=v::values, closures=closures}

(* Add a closure to the current scope. *)
fun addclosure(ENV{values,closures}, c) =
	ENV{values=values, closures=c::closures}

(* Compose two Accesses.  I expect that the first will always be PATH. *)
exception Compose
fun composepath ([i:int],j::rest) = (i+j)::rest
  | composepath (i::a, l) = i::(composepath(a,l))
  | composepath _ = raise Compose
fun compose(PATH a, PATH b) = PATH(composepath(a,b))
  | compose(a, b as KNOWN _ ) = b
  | compose(a, b as CONST _ ) = b
  | compose _ = raise Compose

(* Take an environment and return a function mapping lvars to accesses.
   Possibly this should be fixed to examine the stamps of closures
   to handle multiple paths faster or more intelligently. *)
exception Lookup
fun lookup (ENV{values,closures}) =
    fn lvar =>
	let fun get nil = raise Lookup
	      | get ((CLOSURE{functions,values,closures,...},access)::tl) =
		let fun c (nil,_) = raise Lookup
		      | c (hd::tl,i) = (c(tl,i+1) handle Lookup =>
					get [(hd,compose(access,PATH[i,0]))])
		    fun b (nil,i) = (get tl handle Lookup => c(closures,i))
		      | b (hd::tl,i) = if lvar=hd then compose(access,PATH[i,0])
				       else b(tl,i+1)
		    fun a (nil,i) = b(values,i)
		      | a (hd::tl,i) = if lvar=hd then compose(access,PATH[i])
				       else a(tl,i+1)
		in  case functions
		      of nil => b(values,1)
		       | _ => a(functions,0)
		end
	    fun find nil = get closures
	      | find ((v,access)::tl) =
			if lvar=v then access else find tl
	in  find values
	end

(* Take an environment and return a function mapping closures to accesses.
   Possibly this should be fixed to examine the stamps of closures
   to handle multiple paths faster or more intelligently. *)
fun lookupClosure (ENV{closures,...}) =
    fn CLOSURE{stamp=s,...} =>
	let fun get nil = raise Lookup
	      | get ((CLOSURE{stamp=s',closures,functions,values},access)::tl) =
		let fun c (nil,_) = raise Lookup
		      | c (hd::tl,i) = (c(tl,i+1) handle Lookup =>
					get [(hd,compose(access,PATH[i,0]))])
		in  if s=s' then access
		    else get tl
		    handle Lookup =>
		    case functions
		      of nil => c(closures, 1 + length values)
		       | _ => c(closures, length functions + length values)
		end
	in  get closures
	end

(* Take a closure strategy and return a makenv function using it. *)
fun build strategy =
 fn (e as FN(v,_), env, _) =>
     let val (values,closures,accesses) = strategy (v,env)
	 val c1 = (mkClosure(nil,values,closures), PATH[0,0])
	 val lf = newlabel()
	 val close = KNOWN lf :: accesses
     in  (env0, [(lf,e,ENV{values=nil,closures=[c1]})], close)
     end
  | (APP(FN(w,_),e as FN(v,_)), env, offset) =>
     let val (values,closures,accesses) = strategy (v,env)
	 val c = mkClosure([w],values,closures)
	 val c0 = (c, PATH[offset,0])
	 val c1 = (c, PATH[0,0])
	 val lf = newlabel()
	 val close = KNOWN lf :: accesses
     in  (addclosure(env,c0), [(lf,e,ENV{values=nil,closures=[c1]})], close)
     end
  | (FIX(vl as v::_, el, _), env, offset) =>
     let val (values,closures,accesses) = strategy (v,env)
	 val c = mkClosure(vl,values,closures)
	 val c0 = (c, PATH[offset,0])
	 fun frags(e::re, i) =
		let val ci = (c, PATH[0,~i])
		in  (newlabel(), e, ENV{values=nil,closures=[ci]})::frags(re,i+1)
		end
	   | frags(nil,_) = nil
	 val fgs = frags(el,0)
	 val close = map (KNOWN o #1) fgs @ accesses
     in  (addclosure(env,c0), fgs, close)
     end
  | _ => ErrorMsg.impossible "build in codegen/codenv.sml"

(* Take the current environment and the list of free variables, and return
   the functions among the free variables, the values, and the closures
   which should be copied to obtain the functions. *)
fun findFuncsAndVals (ENV{closures,...}) =
    let fun isFunction v (CLOSURE{functions,...},_) : bool =
		exists (fn w => w=v) functions
	fun getClosure v =
	    let fun get nil = NONE
		  | get (c::tl) = if isFunction v c
					then SOME c else get tl
	    in  get
	    end
    in  fn freeVarList => 
	fold (fn (v,(functions,values,cs)) =>
		if exists (isFunction v) cs then (v::functions,values,cs)
		else (case (getClosure v closures)
		        of NONE => (functions,v::values,cs)
		         | SOME cl => (v::functions,values,cl::cs)))
	     freeVarList
	     (nil,nil,nil)
    end

(* Pure flat closures. *)
fun flat0 freevars =
 let fun strategy_flat0 (v,env) =
     let val free = freevars v
			handle Opt.Freevars => nil (* for inline functions *)
     in  (free,
	  nil,
	  map (lookup env) free)
     end
 in  build strategy_flat0
 end

(* Flat closures except that when two free functions are defined in the same
   closure, only one pointer to the closure is copied. *)
fun flat1 freevars =
 let fun strategy_flat1 (v,env) =
     let val free = freevars v
			handle Opt.Freevars => nil (* for inline functions *)
	 val (_,values,closures) = findFuncsAndVals env free
     in  (values,
	  map #1 closures,
	  map (lookup env) values @ map #2 closures)
     end
 in  build strategy_flat1
 end

(* Take an env and a free variable list, and return the list of variables
   defined in the current scope and the list of variables defined elsewhere.
   It expects that functions defined in the current scope have already
   been identified, and are not passed as part of the list argument. *)
fun partitionByScope (ENV{values,closures}) =
    let fun divide (nil : lvar list) = (nil,nil)
	  | divide (hd::tl) =
		let val (current,other) = divide tl
		in  if exists (fn (w,_) => hd=w) values
		    then (hd::current,other)
		    else (current,hd::other)
		end
    in  divide
    end

exception PathTo
fun pathTo(vars,closure) =
  let fun p (CLOSURE{functions,values,...}) =
	   exists (fn v => let val needed = (fn w => v=w)
			   in  exists needed functions
				orelse exists needed values
			   end)
		  vars
      fun ps nil = raise PathTo
        | ps ((c as CLOSURE{closures,...})::tl) =
		(if p c then c else ps tl handle PathTo => ps closures)
  in  ps [closure]
  end

fun whatelse (_,nil,values,closures) = (values,closures)
  | whatelse (env as ENV{closures=cs,...},needed,values,closures) =
    let val parent as (cl as CLOSURE{stamp=s,...},_) = root cs
    in  if !CGoptions.path
	  then case needed of [v] => (v::values,closures)
	          | _ => let val c = pathTo(needed,cl)
			 in  (values,((c,lookupClosure env c)::closures))
			 end
	else if exists (fn (CLOSURE{stamp=s',...},_) => s=s') closures
	  then (values,closures)
	else (values,parent :: closures)
    end
    handle Root =>
	(ErrorMsg.warn "Free variables and no root closure in codenv";
	 (values, closures))

(* Pure linked closures.
   Note that here you could have slots for parent functions as well
   as a link to the closure itself. *)
fun link0 freevars =
 let fun strategy_link0 (v,env as ENV{closures=cs,...}) =
     let val free = freevars v
			handle Opt.Freevars => nil (* for inline functions *)
	 val (functions,values,closures) = findFuncsAndVals env free
	 val (inCurrentScope,inOtherScope) = partitionByScope env values
	 val parent = [root cs] handle Root => nil
	 val values = functions @ inCurrentScope
     in  (values,
	  map #1 parent,
	  map (lookup env) values @ map #2 parent)
     end
 in  build strategy_link0
 end

(* Linked closures with no unnecessary links to the parent closure and
   free functions of the same closure sharing one link. *)
fun link1 freevars =
 let fun strategy_link1 (v,env as ENV{closures=cs,...}) =
     let val free = freevars v
			handle Opt.Freevars => nil (* for inline functions *)
	 val (_,values,closures) = findFuncsAndVals env free
	 val (inCurrentScope,inOtherScope) = partitionByScope env values
	 val (values,closures) = whatelse(env,inOtherScope,inCurrentScope,closures)
     in  (values,
	  map #1 closures,
	  map (lookup env) values @ map #2 closures)
     end
 in  build strategy_link1
 end

(* Linked closures with no unnecessary links to the parent closure,
   free functions of the same closure sharing one link,
   and no values copied from the current scope that are reachable
   through the closures of the necessary functions. *)
fun link2 freevars =
 let fun strategy_link2 (v,env as ENV{closures=cs,...}) =
     let val free = freevars v
			handle Opt.Freevars => nil (* for inline functions *)
	 val (_,values,closures) = findFuncsAndVals env free
	 val accessibleValues = 
		let val cenv = lookup (ENV{closures=closures,values=nil})
		in  sublist
			(fn v => ((cenv v; true) handle Lookup => false))
			values
		end
	 val (inCurrentScope,inOtherScope) =
		partitionByScope env (diff(values,accessibleValues))
	 val (values,closures) = whatelse(env,inOtherScope,inCurrentScope,closures)
     in  (values,
	  map #1 closures,
	  map (lookup env) values @ map #2 closures)
     end
 in  build strategy_link2
 end

(* Like link1 except that values used in the current scope are
   always copied directly into the closure. *)
fun mix0 freevars =
 let fun strategy_mix0 (v,env as ENV{closures=cs,...}) =
     let val (free,current) = freevars v
			handle Opt.Freevars => (nil,nil) (* for inline functions *)
	 val (functions,values,closures) = findFuncsAndVals env free
	 val currentValues = diff(current,functions)
	 val nestedValues = diff(values,currentValues)
	 val (inCurrentScope,inOtherScope) = partitionByScope env nestedValues
	 val values = currentValues @ inCurrentScope
	 val (values,closures) = whatelse(env,inOtherScope,values,closures)
     in  (values,
	  map #1 closures,
	  map (lookup env) values @ map #2 closures)
     end
 in  build strategy_mix0
 end

fun findNecessaryClosures (ENV{closures,...}) =
    let fun isFunction v (CLOSURE{functions,...},_) =
		exists (fn w => w=v) functions
	fun getClosure v =
	    let fun get nil = NONE
		  | get (c::tl) = if isFunction v c
				  then SOME c else get tl
	    in  get
	    end
	fun includes (CLOSURE{stamp,...},_) =
	    let fun includes0 (CLOSURE{closures,...}) =
		exists (fn CLOSURE{stamp=s',...} => s'=stamp) closures
		  orelse exists includes0 closures
	    in includes0
	    end
    in  fn freeVarList => 
	fold (fn (v,(functions,values,cs)) =>
		if exists (isFunction v) cs then (v::functions,values,cs)
		else case getClosure v closures
		       of NONE => (functions,v::values,cs)
		        | SOME c =>
			(v::functions,
			 values,
			 if exists (includes c) (map #1 cs) then cs
			 else c :: sublist (fn d => not(includes d (#1 c))) cs)
	      )
	     freeVarList
	     (nil,nil,nil)
    end

(* An approximation to the locally minimal version. *)
fun link3 freevars =
 let fun strategy_link3 (v,env) =
     let val free = freevars v
			handle Opt.Freevars => nil (* for inline functions *)
	 val (_,values,closures) = findNecessaryClosures env free
	 val (inCurrentScope,inOtherScope) = partitionByScope env values
	 val values =
		let val cenv = lookup (ENV{closures=closures,values=nil})
		in  sublist
			(fn v => ((cenv v; false) handle Lookup => true))
			inCurrentScope
		end
	 val (values,closures) = whatelse(env,inOtherScope,values,closures)
     in  (values,
	  map #1 closures,
	  map (lookup env) values @ map #2 closures)
     end
 in  build strategy_link3
 end



fun codenv e =
 (case !CGoptions.closureStrategy
    of 0 => (env0, flat0 (Opt.free e))
     | 1 => (env0, flat1 (Opt.free e))
     | 2 => (env0, link0 (Opt.free e))
     | 3 => (env0, link1 (Opt.free e))
     | 4 => (env0, link2 (Opt.free e))
     | 5 => (env0, link3 (Opt.free e))
     | 6 => (env0, mix0 (Opt.mix0free e))
     | _ => (env0, flat0 (Opt.free e))
 ) 
end (* functor Codenv *)
