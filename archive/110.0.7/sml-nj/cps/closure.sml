(* Copyright 1996 by Bell Laboratories *)
(* closure.sml *)

(****************************************************************************
 *                                                                          *
 * NOTE: debugging code has been commented out as follows:                  *
 *           (***> ...code... <***)                                         *
 *                                                                          *
 ****************************************************************************)

functor Closure(MachSpec : MACH_SPEC) : CLOSURE = struct

local 
  open CPS Access AllocProf SortedList
  structure LV = LambdaVar
  structure CGoptions = Control.CG
  val OFFp0 = OFFp 0
  val saveLvarNames = LV.saveLvarNames
  val dupLvar = LV.dupLvar
  val mkLvar = LV.mkLvar
  fun inc (ri as ref i) = ri := i+1
in 

(**********************************************************************
 * Miscellaneous utility functions.                                   *
 *                                                                    *
 * NOTE: An important property of partition and sublist is that they  *
 *  take sorted lists to sorted lists.                                *
 **********************************************************************)
fun partition f l = foldr (fn (e,(a,b)) => if f e then (e::a,b) else (a,e::b))
			 (nil,nil) l

fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl nil = nil
  in  subl
  end

fun formap f =
  let fun iter(nil,_) = nil
	| iter(hd::tl,i) = f(hd,i)::iter(tl,i+1)
  in  iter o (fn l => (l,0))
  end

val error = ErrorMsg.impossible

val zip = ListPair.zip

val closureLvar = 
  let val save = (!saveLvarNames before saveLvarNames := true)
      val closure = LV.namedLvar(Symbol.varSymbol "closure")
   in (saveLvarNames := save; fn () => dupLvar closure)
  end

(**********************************************************************
 * Static environments.                                               *
 **********************************************************************)
datatype closureRep = CR of int * closure 
withtype closure = {functions : (lvar * lvar) list,
		    values : lvar list,
		    closures : (lvar * closureRep) list,
		    stamp : lvar}

datatype object = Value of cty
		| Function of {label:lvar,free:lvar list}
		| Closure of closureRep
                | Callee of value * value list

datatype access = Direct
		| Path of (lvar * accesspath)

abstype env = Env of lvar list *                    (* values *)
	             (lvar * closureRep) list *     (* closures *)
                     object Intmap.intmap           (* what map *)
with
exception NotBound
fun emptyEnv() = Env(nil,nil,Intmap.new(32,NotBound))

(* Update an environment *)
fun augment(m as (v,obj),e as Env(valueL,closureL,whatMap)) =
      (Intmap.add whatMap m;
       case obj
         of Value _ => Env(v::valueL,closureL,whatMap)
          | Closure cr => Env(valueL,(v,cr)::closureL,whatMap)
          | _ => e)
fun augmentV((v,t),Env(valueL,closureL,whatMap)) =
      (Intmap.add whatMap(v,Value t);
       Env(v::valueL,closureL,whatMap))
fun addCallee(Env(_,_,whatMap),(v,c)) = Intmap.add whatMap (v,Callee c)

(* Return the immediately enclosing closure, if any.  This is a hack. *)
fun getClosure (Env(_,closureL,_)) =
  let fun getc ([z]) = SOME z
	| getc (_::tl) = getc tl
	| getc nil = NONE
  in  getc closureL
  end

(* Return all the closures currently in the environment. *)
fun getClosures (Env(_,closureL,_)) = 
  let fun extract(_,CR(_,{closures,...})) = closures
      val level2 = List.concat (map extract closureL)
   in closureL@level2
  end 

(**********************************************************************
 * Environment printing, for debugging.                               *
 **********************************************************************)
val pr = Control.Print.say
val vp = pr o LV.lvarName
fun plist p l = (app (fn v => (pr " "; p v)) l; pr "\n")
val ilist = plist vp
fun sayv(VAR v) = vp v
  | sayv(LABEL v) = (pr "(L)"; vp v)
  | sayv(INT i) = (pr "(I)"; pr(Int.toString i))
  | sayv(INT32 i) = (pr "(I32)"; pr(Word32.toString i))
  | sayv(REAL r) = pr r
  | sayv(STRING s) = (pr "\""; pr s; pr "\"")
  | sayv(OBJECT _) = pr "**OBJECT**"
  | sayv(VOID) = pr "**VOID**"
val vallist = plist sayv

fun printEnv(Env(valueL,closureL,whatMap)) =
  let fun ip (i : int) = pr(Int.toString i)
      val tlist = plist (fn (a,b) => (vp a; pr "/"; sayv(LABEL b)))
      fun fp(v,Function{label,free}) =
	(vp v; pr "/known "; sayv(LABEL label); pr " -"; ilist free)
        | fp _ = ()
      fun cp (v,Callee(v',vl)) =
	(vp v; pr "/callee "; sayv v'; pr " -"; vallist vl)
        | cp _ = ()
      fun p(indent,l,seen) =
	let fun c(v,CR(offset,{functions,values,closures,stamp})) =
	      (indent(); pr "Closure "; vp v; pr "/"; ip stamp;
	       pr " @"; ip offset;
	       if member seen stamp
	       then pr "(seen)\n"
	       else (pr ":\n";
		     case functions
		       of nil => ()
		        | _ => (indent(); pr "  Funs:"; tlist functions);
		     case values
		       of nil => ()
		        | _ => (indent(); pr "  Vals:"; ilist values);
		     p(fn() => (indent();pr "  "),closures,enter(stamp,seen))))
	in  app c l
	end
  in  pr "Values:"; ilist valueL;
      pr "Closures:\n"; p(fn () => (),closureL,nil);
      pr "Known function mapping:\n"; Intmap.app fp whatMap;
      pr "Callee-save continuation mapping:\n";
      Intmap.app cp whatMap
  end


exception Lookup of lvar * env
(************************************************************************
 * whatIs: return type of object bound to an lvar in an environment.    *
 ************************************************************************)
fun whatIs(env as Env(_,_,whatMap),v) =
  Intmap.map whatMap v
    handle NotBound => raise Lookup(v,env)

(************************************************************************
 * whereIs: find the access path to a value in an environment.          *
 ************************************************************************)
fun whereIs(env as Env(valueL,closureL,whatMap),target) =
  let fun bfs(nil,nil) = raise Lookup(target,env)
	| bfs(nil,next) = bfs(next,nil)
	| bfs((h,CR(offset,{functions,values,closures,stamp}))::m,next) =
            let fun cls(nil,i,next) = bfs(m,next)
		  | cls((v,cr)::t,i,next) =
		    if target=v
		    then h(SELp(i-offset,OFFp 0))
		    else cls(t,i+1,
			     (fn p => h(SELp(i-offset,p)),cr)::next)
		fun vls(nil,i) = cls(closures,i,next)
		  | vls(v::t,i) =
		    if target=v
		    then h(SELp(i-offset,OFFp 0))
		    else vls(t,i+1)
		fun fns(nil,i) = vls(values,i)
		  | fns((v,l)::t,i) =
		    if target=v
		    then h(OFFp(i-offset)) (* possible OFFp 0 *)
		    else fns(t,i+1)
	    in  if target=stamp
		then h(OFFp(~offset)) (* possible OFFp 0 *)
		else fns(functions,0)
	    end
      fun search closures =
	let val (v,p) = bfs(map (fn (v,cr) => ((fn p => (v,p)),
					       cr)) closures,
			    nil)
	in  Path(v,p)
	end
      fun lookC ((v,_)::tl) =
	    if target=v then Direct else lookC tl
	| lookC nil = search closureL
      fun lookV (v::tl) =
	    if target=v then Direct else lookV tl
	| lookV nil = search closureL
  in  case whatIs(env,target)
	of Function _ => Direct
	 | Callee _ => Direct
	 | Closure _ => lookC closureL
	 | Value _ => lookV valueL
  end

end (* abstype env *)

(* Bind the offset of a closure to a variable in an environment *)
fun offset(i,CR(offset,x),v,env) =
  augment((v,Closure(CR(offset+i,x))),env)

(* Perhaps we should change the representation of closures
   to make this faster. *)
fun inClosure (c,CR(_,{functions,values,closures,stamp})) v =
  (v=c
   orelse v=stamp
   orelse List.exists (fn (w,_) => v=w) functions
   orelse List.exists (fn w => v=w) values
   orelse List.exists (fn z => inClosure z v) closures)


(**********************************************************************
 * Simple closure strategies.                                         *
 **********************************************************************)
local
fun flat(env,free) =
 foldr (fn (v,(vls,cls)) =>
        let val obj = whatIs(env,v)
	in  case obj
              of Value _ => (v::vls,cls)
               | Closure cr => (vls,(v,cr)::cls)
	       | _ => error "unexpected vc in cps/closure.sml"
	end) (nil,nil) free
fun link(env,free) =
  case getClosure(env)
    of NONE => flat(env,free)
     | SOME z =>
         let val notIn = sublist (not o (inClosure z)) free
	     val (values,closures) = flat(env,notIn)
	 in  if length(notIn) = length(free) (* Does adding the
                                                closure help? *)
	     then (values,closures)          (* NO *)
	     else (values,z::closures)       (* YES *)
	 end
in
fun closureStrategy(functions,free,env) =
  let val (values,closures) =
        case !CGoptions.closureStrategy
	  of 3 => link(env,free)
	   | 2 => link(env,free)
	   | _ => flat(env,free)
      val cname = closureLvar()
  in  (cname,{cname=cname,
   	      cr=CR(0,{functions=functions,
		       values=values,closures=closures,stamp=cname}),
	      contents=(map (LABEL o #2) functions)
	               @ (map VAR values)
	               @ (map (VAR o #1) closures)})
  end
end (* local *)


(**********************************************************************
 * sameClosureOpt: if two free variables are functions from the same  *
 * closure, just one of them is sufficient to access both.            *
 **********************************************************************) 
fun sameClosureOpt(free,env) =
case !CGoptions.closureStrategy
  of 0 => free (* flat without aliasing *)
   | 2 => free (* linked without aliasing *)
   | _ => (* all others have aliasing *)
  let val mapping = map (fn v => let val obj = whatIs(env,v)
				 in  (v,obj)
				 end) free
      fun uniq ((hd as (v,Closure(CR(_,{stamp,...}))))::tl) =
	let val m' = uniq tl
	in  if List.exists (fn (_,Closure(CR(_,{stamp=stamp',...}))) => stamp=stamp'
			| _ => false) m'
	    then m' else hd::m'
	end
	| uniq (hd::tl) = hd::uniq tl
	| uniq nil = nil
  in  map #1 (uniq mapping)
  end

local

fun selFltHeader(i,u,x,ct,ce) = 
  case (ct,MachSpec.unboxedFloats)
   of (FLTt,true) => (let val v = mkLvar()
                       in SELECT(i,u,v,BOGt,(SELECT(0,VAR v,x,ct,ce)))
                      end)
   | _ => SELECT(i,u,x,ct,ce)   

fun follow (rootvar,t) =
  let val rec follow0 =
	fn (v,OFFp off,h) =>
		h o (fn ce => OFFSET(off,VAR v,rootvar,ce))
	 | (v,SELp(i,OFFp 0),h) =>
                h o (fn ce => selFltHeader(i,VAR v,rootvar,t,ce))
	 | (v,SELp(i,p),h) =>
		let val w = mkLvar()
		 in  follow0(w,p,h o (fn ce => SELECT(i,VAR v,w,BOGt,ce)))
		end handle Bind => error "follow in closure"
  in  follow0
  end
in
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
fun access(VAR rootvar,(env,header)) =
  let val what = whatIs(env,rootvar)
    
      val t = case what
	       of Callee _ => error "Callee in fixAccess"
	        | Function _ => error "Function in fixAccess"
	        | Value ct => ct
                | _ => BOGt
   in
      case whereIs(env,rootvar)
	of Direct => (env,header)
	 | Path(start,path) =>
	    let val header = 
                  follow (rootvar,t) (start,path,header)
		val env = augment((rootvar,what),env)
	    in  if not(!CGoptions.allocprof) then (env,header)
		else (env,header o profLinks(lenp path))
	    end
  end
  | access(_, y) = y
in  foldr access (env,fn x => x) args
end

fun fixArgs(args,env) =
  let fun fixArgs0 (nil,env,h) = (nil,env,h)
	| fixArgs0 (hd::tl,env,h) =
            (case hd
	       of VAR rootvar =>
		  let val what = whatIs(env,rootvar)
                      val t = case what of Value ct => ct
                                         | _ => BOGt
 		   in case what
		       of Callee(label,extra) =>
		            let val args' = (label::extra)@tl
				 val (env,h') = 
                                    fixAccess(args',env)
			     in  (args',env,h o h')
		            end
		        | Function _ => error "Function in fixArgs"
			| _ =>
			  (case whereIs(env,rootvar)
			     of Direct =>
				  let val (args',env,h) = fixArgs0(tl,env,h)
				   in (hd::args',env,h)
				  end
 			      | Path(start,path) =>
				  let val h = follow (rootvar,t) 
                                                  (start,path,h)
				      val env = augment((rootvar,what),env)
				      val (args',env,h) = fixArgs0(tl,env,h)
				   in if not(!CGoptions.allocprof)
				      then (hd::args',env,h)
				      else (hd::args',env,
					    h o profLinks(lenp path))
				  end)
		  end
                | _ => let val (args',env,h) = fixArgs0(tl,env,h)
		       in  (hd::args',env,h)
		       end)
   in  fixArgs0(args,env,fn x => x)
   end (* fixArgs *)
end (* local *)



(****************************************************************************
 * recordEl: Find the complete access paths for elements of a record.       *
 * Return a header for profiling purposes if needed.                        *
 ****************************************************************************)
fun recordEl(l,env) =
if not(!CGoptions.allocprof)
then (map (fn (VAR v,p) => 
       	    (case whereIs(env,v)
	       of Direct => (VAR v,p)
	        | Path(start,path) => (VAR start, combinepaths(path,p)))
            | vp => vp)
          l,
      fn x => x)
else
let val (rl,cl) = 
      foldr (fn ((VAR v,p),(l,cl)) =>
	         let val (m,cost) =
                       case whereIs(env,v)
			 of Direct => ((VAR v,p),0)
			  | Path(start,path) =>
			      ((VAR start, combinepaths(path,p)),
			       lenp path)
		 in  (m::l,cost::cl)
		 end
	      | (m,(l,cl)) => (m::l,0::cl))
	    (nil,nil) l
in  (rl,profRecLinks cl)
end



(****************************************************************************
 * freeAnalysis: Take a free variable list and:                             *
 * (1) replace knownfuncs by their free variables;                          *
 * (2) replace callee-save continuations by their extra arguments.          *
 ****************************************************************************)
local
fun clean l = 
let fun vars(l, VAR x :: rest) = vars(x::l, rest)
      | vars(l, _::rest) = vars(l,rest)
      | vars(l, nil) = l
 in vars(nil,l)
end
in
fun freeAnalysis(free,env) =
  foldr (fn (v,l) =>
	  case whatIs(env,v)
	      of Callee(VAR v',extra) => (** outside case **)
                                   (** the clean is unnecessary **)
		     merge(l,enter(v',uniq(clean extra)))
	       | Callee(LABEL _,extra) => (** inside case **)
		     merge(l,uniq(clean extra))
	       | Function{free,...} => merge(free,l)
	       | _ => enter(v,l))
	[] free

fun freeTmpAnalysis(free,env) =
  foldr (fn (v,l) =>
         ((case whatIs(env,v)
              of Callee(VAR v',extra) => (** outside case **)
                                   (** the clean is unnecessary **)
                     merge(l,enter(v',uniq(clean extra)))
               | Callee(LABEL _,extra) => (** inside case **)
                     merge(l,uniq(clean extra))
               | Function{free,...} => merge(free,l)
               | _ => enter(v,l))
           handle Lookup _ => enter(v,l)))
        [] free

end


fun partBindings fl = 
  let fun h((fe as (ESCAPE,_,_,_,_))::r,el,kl,rl,cl) = h(r,fe::el,kl,rl,cl)
        | h((fe as (KNOWN,_,_,_,_))::r,el,kl,rl,cl) = h(r,el,fe::kl,rl,cl)
        | h((fe as (KNOWN_REC,_,_,_,_))::r,el,kl,rl,cl) = 
                                                  h(r,el,fe::kl,fe::rl,cl)
        | h((fe as (KNOWN_CONT,_,_,_,_))::r,el,kl,rl,cl) =
                                                  h(r,el,fe::kl,rl,cl)
        | h((fe as (KNOWN_TAIL,_,_,_,_))::r,el,kl,rl,cl) =
                                                  h(r,el,fe::kl,rl,cl)
        | h((fe as (CONT,_,_,_,_))::r,el,kl,rl,cl) = h(r,el,kl,rl,fe::cl)
        | h(_::r,el,kl,rl,cl) = error "partBindings in closure phase 231"
        | h([],el,kl,rl,cl) = (el,kl,rl,cl)
   in h(fl,[],[],[],[])
  end


(**********************************************************************
 * closeCPS: MAIN FUNCTION                                            *
 **********************************************************************)
fun closeCPS(fk,f,vl,cl,ce) =
let

val baseEnv = emptyEnv()

local val maxgpregs = MachSpec.numRegs
      val maxfpregs = MachSpec.numFloatRegs - 2  (* need 1 or 2 temps *)

fun numgp l = 
  let fun h (FLTt::z) = h(z)
        | h (CNTt::z) = 1 + MachSpec.numCalleeSaves + h(z)
        | h (_::z) = 1 + h(z)
        | h [] = 0
   in h l
  end

(*** count the number of fp registers needed for a list of lvars ***)
fun numfp l = 
  let fun h (FLTt::z) = 1 + h(z)
        | h (CNTt::z) = MachSpec.numFloatCalleeSaves + h(z)
        | h (_::z) = h(z)
        | h [] = 0
   in h l
  end

in val numCSregs = MachSpec.numCalleeSaves
   val unboxedfloat = MachSpec.unboxedFloats


   fun exceedLimit(cl) =
     if unboxedfloat then 
       (((numgp cl) >= maxgpregs) orelse ((numfp cl) >= maxfpregs))
     else (length cl >= (maxgpregs - numCSregs))

   fun wrapFltHeader(t,p) =
     if (not unboxedfloat) then (p,fn ce => ce)
     else (case t
            of FLTt => let val v = mkLvar()
                          in (v,fn ce => 
                                  RECORD(RK_FBLOCK,[(VAR p, OFFp0)],v,ce))
                         end
             | _ => (p,fn ce => ce))

end

val ((fk,f,vl,cl,ce),nfreevars) = FreeClose.freemapClose(fk,f,vl,cl,ce)

local

val (iscont0,ebinfo0) =
 (case numCSregs
    of 0 => (fn _ => false, 
             fn _ => error "check EBinfo when callee=0 in closure.sml")
     | _ => ContMap.contmap(fk,f,vl,cl,ce))

val (freevars,isEscape,isKnown) = FreeMap.freemapClose ce

in    
val ebinfo = ebinfo0
(** A continuation will be known if the function that it gets
    passed to is inlined.  If so, we just treat it as a regular
    known function. **)
fun isCallee v = (iscont0 v) andalso (not(isKnown v))

val (freevars,isEscape) = (freevars,isEscape)

end (* local *)

val extraConst =
  let fun ec(0) = nil
	| ec(k) = (INT 0)::(ec(k-1))
   in ec(numCSregs)
  end

(***********************************************************************
 * addExtra: look at the formal arguments of a function, and replace   *
 * parameters to be treated as callee-save continuations by new        *
 * parameters for the continuation and its extra arguments.            *
 ***********************************************************************)
local
fun extraLvar(0) = nil
  | extraLvar(k) =  mkLvar()::extraLvar(k-1)

fun addExtra1(args,cl0) =
  foldr (fn ((a,t),(al,cl,el)) =>
	 if (t = CNTt)
	 then let val a' = LV.dupLvar a
		  val el' = extraLvar(numCSregs)
                  val cl' = (map (fn _ => BOGt) el')
	      in  if not(null el)
			 (* A function can have 1 or 0 continuation
			    arguments -- 0 if it is a continuation. *)
		  then error "closure/addExtra: >1 continuation"
		  else (addCallee(baseEnv,(a,(VAR a',map VAR el')));
			(a'::(el'@al),CNTt::(cl'@cl),el'))
	      end
	 else  (a::al,t::cl,el)) (nil,nil,nil) (zip(args,cl0))

fun addExtra2(args,cl0) =
  foldr (fn ((a,t),(al,cl,el)) => (a::al,t::cl,el)) (nil,nil,nil) (zip(args,cl0))

in
val addExtra = if (numCSregs > 0) then addExtra1
               else addExtra2
end (* local *)

fun addinc(c,label,extra) = addCallee(baseEnv,(c,(LABEL label,extra)))

(***>
val alphac = CGoptions.alphac
val unrebind = fn x => if !alphac then UnRebind.unrebind x else x
<***)



(**********************************************************************
 * makenv: Create the environments for functions in a FIX.            *
 **********************************************************************)
fun makenv(initEnv, bindings : function list, extraArgs) =
let

(***>
    fun COMMENT f = if !CGoptions.comment then (f(); ()) else ()
    val _ = COMMENT(fn() => (pr "BEGINNING MAKENV.\nFunctions: ";
			     ilist (map #2 bindings);
			     pr "Initial environment:\n";
			     printEnv initEnv; pr "\n"))

    val freevars =
      (fn v => let val free = freevars v
	       in  COMMENT(fn() => (pr "Free in "; vp v; pr ":"; ilist free));
		   free
	       end)
<***)


fun COMMENT f = if !CGoptions.comment then (f(); ()) else ()
fun checkfree(v) = 
  let val free = freevars v
      val (nfree,loopv) = nfreevars v
   
      val _ = if (free <> nfree) then 
        (COMMENT(fn() => (pr "^^^^ wrong free variable subset ^^^^ \n"; 
                          pr "Free in "; vp v; pr ":"; ilist free;
                          pr "NFree in "; vp v; pr ":"; ilist nfree;
                          pr "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ \n")))
      else ()

      fun subset(l,r) =  (difference(l,r) = [])
      val _ = case loopv 
               of NONE => ()
                | SOME sfree =>
                  (if subset (sfree,nfree) then ()
                   else  
                 (COMMENT(fn() => (pr "****wrong free variable subset*** \n"; 
                                   pr "Free in "; vp v; pr ":"; ilist nfree;
                                   pr "SubFree in "; vp v; pr ":";ilist sfree;
                                   pr "*************************** \n"))))
   in () 
  end

val _ = app checkfree (map #2 bindings)

fun get_cty v = case whatIs(initEnv,v) of Value t => t 
                                        | _ => BOGt
fun isFlt v = if not unboxedfloat then false
              else (case (get_cty v) of FLTt => true
                                      | _ => false)

(* Separate function bindings into those that escape, those which are
   known functions, and callee-save continuations. *)

(*** didn't consider anything about calleesaves = 0 yet ***)

val (escapeB,knownB,recB,calleeB) = partBindings(bindings)

val (escapeB,calleeB) = 
      if (numCSregs > 0) then (escapeB,calleeB)
      else (escapeB@calleeB,[])

(***>>
val (escapeB,knownB) = partition (isEscape o #2) bindings
val (calleeB,escapeB) = partition (isCallee o #2) escapeB
<<***)
val escapeV = uniq(map #2 escapeB)
val calleeV = uniq(map #2 calleeB)
val knownV = uniq(map #2 knownB)
val knownlvar = member knownV

(*** check whether the assumption No. 2 is valid ***)
val fixKind = 
  case (escapeB,knownB,calleeB,recB) 
   of ([],_,[],_) => KNOWN
    | ([],[],[v],[]) => CONT
    | (_,_,[],_) => ESCAPE
    | _ => (COMMENT(fn() => 
               (pr "^^^ Assumption No.2 is violated in closure phase  ^^^\n";
                pr "KNOWN bindings: "; ilist (map #2 knownB);
                pr "ESCAPE bindings: "; ilist (map #2 escapeB);
                pr "CONT bindings: "; ilist (map #2 calleeB);
                pr "KNOWN_REC bindings: "; ilist (map #2 recB);
                pr "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ \n")); ESCAPE)

(***>
val _ = COMMENT(fn() => (pr "Known functions:"; ilist (map #2 knownB)))
<***)

(* Initial processing of the known function bindings. *)
val knownB =
 map (fn (_,v,args,cl,body) =>
        let val (free,lpv) = nfreevars v
            val (free,spill) = (free,NONE)
(*              case lv 
               of NONE => (free,NONE)
                | SOME l => (l,SOME (difference(free,l))) *)
            (* Separate the free variable list into known functions 
             * defined in this FIX and other free variables. 
             *)
            val (fns,other) = partition knownlvar free
         in ({v=v,args=args,cl=cl,body=body,other=other,
              spill=spill},length fns,fns)
        end) knownB

(****************************************************************************
 * closeCallGraph: compute the transitive closure of the call graph of a    *
 * set of (possibly recursive) functions.                                   *
 ****************************************************************************)
type info = {v:lvar,args:lvar list,cl:cty list,body:cexp,other:lvar list, 
             spill:lvar list option}
fun closeCallGraph g =
  let fun getNeighbors l =
        foldr (fn (({v,...}:info,_,nbrs),n) =>
	      if member l v then merge(nbrs,n)
		  else n) l g
      fun f ((x,len,nbrs),(l,change)) =
	  let val nbrs' = getNeighbors nbrs
	      val len' = length nbrs'
	  in  ((x,len',nbrs')::l,change orelse len<>len')
	  end
      val (g',change) = foldr f (nil,false) g
  in  if change then closeCallGraph g' else g'
  end

(* Compute the closure of the call graph of the known functions. *)
val knownB = closeCallGraph knownB

(*** Collects all spilled variables from the known functions ***)
val collected = 
  foldmerge (map ((fn NONE => [] | SOME l => l) o #spill o #1) knownB)

(* See which known functions require a closure, pass 1. *)
local fun gatherNbrs l init =
        foldr (fn (({v,other,...}:info,_,_),free) =>
	      if member l v then merge(other,free)
		  else free) init knownB
in
val (knownB,fullClosure) = foldr
 (fn ((x as {v,args,other,cl,spill,...}:info,_,fns),(k,fullClosure)) =>
  let
      (* Get the combined list of free variables of all the functions
         reachable in the call graph. *)
      val free = gatherNbrs fns other
      val len = length free

      (* Remove any escaping functions of the FIX from the free variable
         list and mark that the function requires the closure. *)
      local val free' = difference(free,escapeV)
	    val len' = length free'
      in
      val callc = (len<>len') orelse (member collected v)
      val free = free'
      val len = len'
      end

      val needc = case spill of NONE => false  
                              | SOME _ => true

      (* Remove any callee-save continuations of the FIX from the free
         variable list and mark that the function requires the continuation
	 extra arguments *)
      local val free' = difference(free,calleeV)
	    val len' = length free'
      in
      val extrac = (len<>len')
      val free = free'
      val len = len'
      end

      (* Replace known functions defined in other FIX'es by their free
         variables, and callee-save continuation variables defined in
	 other FIX'es by their extra arguments. *)
      val free = freeAnalysis(free,initEnv)

      (* If the free list contains two functions from the same
         closure, we only need one pointer to the closure. *)
      val free = sameClosureOpt(free,initEnv)

      (* If the function has too many extra arguments to fit into
         registers, then we must put them in the closure. *)

      val len = length free
      val allcl = cl@(map get_cty free)
      val callc = callc orelse
	          (exceedLimit(allcl) andalso len > 1)

      (* for calleesave only *)
      val callc = callc orelse extrac
   in ((x,free,callc,needc,fns)::k,fullClosure orelse extrac)
  end) (nil,false) knownB
end (* local *)

(*** Make the collected free variables more concrete. ***)
val collected = difference(difference(collected,knownV),escapeV)
val collected = freeAnalysis(collected,initEnv)

(* See which known functions require a closure, pass 2. *)
local fun checkNbrs1 l init =
        foldr (fn (({v,...}:info,_,callc,_,_),c) =>
	        c orelse (callc andalso (member l v))) init knownB

      fun checkNbrs2 l init =
        foldr (fn (({v,...}:info,_,_,needc,_),c) =>
	        c orelse (needc andalso (member l v))) init knownB
in
val (knownB,collected) = foldr
 (fn (({v,args,cl,body,spill,...}:info,free,callc,needc,fns),(l,c)) =>
   let val callc = checkNbrs1 fns callc
       val needc = checkNbrs2 fns needc
       val (free,collects) = 
         case (callc,spill,collected) 
          of (true,_,_) => (nil,merge(free,c))
           | (_,SOME (_::_),_::_) => (free,merge(free,c))
           | _ => (free,c)

       val label = LV.dupLvar v
   in  ({v=v,args=args,cl=cl,body=body,free=free,label=label,
	 callc=callc orelse needc}::l,collects)
   end) (nil,collected) knownB
end (* local *)

(***>
val _ = COMMENT(fn() => (pr "Escaping functions:"; ilist (map #2 escapeB)))
<***)

(* Get the combined list of the free variables of all the escaping functions
   of the FIX. *)
val escapeFree =
 let val f = foldr (fn (v,f') =>
		         let val (f'',_) = nfreevars v
			 in  merge(f'',f')
			 end)
                      [] escapeV
 in  remove(escapeV, f)
 end

(* Decide on labels for the escaping functions. *)
val escapeB = map (fn (k,v,a,cl,b) => (k,v,LV.dupLvar v,a,cl,b)) escapeB

(* Replace knownfuncs defined in this FIX with their free variables. *)
local val (fns,other) = partition knownlvar escapeFree
in
val escapeFree : lvar list =
  foldr (fn ({v,free,...},b) =>
	if member fns v
	    then merge(free,b)
	else b)
  other knownB
end (* local *)

(* Remove callee-save continuations defined in this FIX, and mark that
 * the closure should contain all free variables of the continuations. 
 *)
local val contlvar = member calleeV
in
val fullClosure = fullClosure orelse (List.exists contlvar escapeFree)
val escapeFree = difference(escapeFree,calleeV)
end (* local *)

(* Add the free variables of knownfuncs in this FIX which were spilled into
 * the closure. 
 *)
val escapeFree = merge(collected,escapeFree)

(* Replace knownfuncs defined elsewhere with their free variables, 
 * and escaping functions defined elsewhere with their closures, and 
 * callee-save continuations with their extra arguments. 
 *)
val escapeFree : lvar list =
  let val escapeFree = freeAnalysis(escapeFree,initEnv)
      val escapeFree = sameClosureOpt(escapeFree,initEnv)
  in  escapeFree
  end


(***>
val _ = COMMENT(fn() => (pr "Callee-save continuations:";
                         ilist (map #2 calleeB)))
<***)

(* Get the combined list of the free variables of all the callee-save
 * continuations of the FIX. 
 *)
val calleeFree : lvar list =
 let val f = foldr (fn (v,f') =>
		   let val (f'',_) = nfreevars v
		   in  merge(f'',f')
		   end)
                  [] calleeV
 in  remove(escapeV,remove(calleeV,f))
 end

(* Decide on labels for the callee-save continuations. *)
val calleeB = map (fn (_,v,a,cl,b) => (v, LV.dupLvar v,a,cl,b)) calleeB

(* Replace knownfuncs defined in this FIX with their free variables. *)
val calleeFree : lvar list =
  let val (fns,other) = partition knownlvar calleeFree
  in  foldr (fn ({v,free,...},b) =>
	    if member fns v
		then merge(free,b)
	    else b) other knownB
  end

val calleeFree : lvar list =
  let val calleeFree = freeAnalysis(calleeFree,initEnv)
      val calleeFree = sameClosureOpt(calleeFree,initEnv)
  in  calleeFree
  end


(**********************************************************************
 * CALLEE-SAVE REGISTER TARGETING                                     *
 **********************************************************************)
local
(** TJ comments are delimited (** ... **). **)

(** There is a problem with closures and aliasing: each function in the
    closure is a name for the closure.  This has some effect on all the
    following functions. **)

fun in'(v,nil) = false
  | in'(v:int,hd::tl) = if v = hd then true else in'(v,tl)

(* Look through a list of enclosing closures to see if we can use them.
 *    retract : lvar list * ((lvar * (lvar * object)) list) -> lvar list 
 *)
fun retract(fl,cl) = (* fl = free list, cl = closure list *)
  let (** How many of fl are reached from the closure? **)
      fun weight (x as (v,_)) =
        let val t = sublist (inClosure x) fl
        in  (** Return how many, which ones, and the closure **)
	    (length t,t,v)
        end

      (** The closures that reach at least one from fl **)
      val clinfo = sublist (fn (k,_,_) => (k > 0)) (map weight cl)

      (** The closures that reach at least 4 from fl,
          OR that are in fl themselves.
          Makes the previous filter bogus -- might as well eliminate it.
	  These closures might still reach values that are not in fl,
	  so in general it is NOT safe for space to use them. **)
      val clinfo = sublist (fn (k,_,v) => (k > 3) orelse (in'(v,fl))) clinfo

      (** Sort the closures by their usefulness.  Unfortunately, this
          sorts into REVERSE order, i.e. the least useful appear at the
          front of the returned list. **)
      val op btr = fn ((k1,_,_),(k2,_,_)) => (k1 > (k2 : int))
      val clinfo = (Sort.sort op btr) clinfo

      (** Consider each closure in turn.  If it looks like it will help
          us, subtract the reachable elements from fl and add the
	  closure itself. **)
      fun repclos (fl,nil) = fl
        | repclos (fl,(_,t,v)::tl) = 
             let val t' = sublist (fn x => in'(x,fl)) t
	     in if ((length(t')) > 3) orelse in'(v,fl)
                then repclos(enter(v,difference (fl,t')),tl)
                else repclos(fl,tl)
             end
   in repclos(fl,clinfo)
  end

val method = 0 (* or !CGoptions.misc3 *)

local
(** If a non-closure free variable is reachable from a closure free variable,
    don't bother to include it in the new closure. **)
fun thinner(fl,cl) =
  let val clinfo = sublist (fn (v,_) => in'(v,fl)) cl

      (** inClinfo(x) iff x is reachable from clinfo. **)
      fun inClinfo z =
         let fun inl nil = false
               | inl (hd::tl) = (inClosure hd z) orelse inl tl
         in  inl clinfo
         end
  in  (** clinfo plus elements of fl not reachable from clinfo. **)
      (merge(uniq(map #1 clinfo),sublist (not o inClinfo) fl))
  end        (**** clinfo may be redundant here ****)
in
(** NOT safe for space unless all of the cl can be included safely
    (see filter0 in calleealloc). **)
fun preproc(fl,cl) = 
  let val m = length(fl)
      (** inImmedAll(v,e) iff all of fl is reachable from e **)
      fun inImmedAll x =
	      (m = length(sublist (inClosure x) fl))
      (** If all of the free variables are reachable from a single closure,
          just use that one.  Otherwise, do thinner or retract. **)
      fun h nil = if method <> 2 then thinner(fl,cl) else retract(fl,cl)
        | h ((x as (v,_))::tl) = if inImmedAll x then [v] else h(tl)
   in h cl
  end 
end (* local *)

(** NOT safe for space unless all of the cl can be included safely
    (see filter0 in calleealloc). **)
(** newfl = list of free variables,
    cl = list of closures,
    k = a threshold value **)
fun preproc2(newfl,cl,k) = 
  let (** how many of newfl are reachable from a closure? **)
      fun weight (x as (v,_)) =
        let val t = sublist (inClosure x) newfl
	in  (** Return how many, which ones, and the closure. **)
	    (length t,t,v)
        end
      val m = length(newfl)

      (** The closures that reach at least one from newfl. **)
      (** In fact we only consider cases where k > 1, might as well
         filter out k=1 here. **)
      val clinfo = sublist (fn (k,_,_) => (k>0))  (map weight cl)

      (** Sort the closures by their usefulness.  Unfortunately, this
          sorts into REVERSE order, i.e. the least useful appear at the
          front of the returned list.  Also, we only care about the
	  MOST useful one, ought to be doing an O(n) max operation
	  instead of a sort. **)
      val op btr = fn ((k1,_,_),(k2,_,_)) => (k1 > (k2 : int))
      val clinfo = (Sort.sort op btr) clinfo

   in case clinfo of 
        nil => (** None of the free variables appear in closures. **)
	    (newfl,NONE)
      | (j,t,v)::_ =>
	    (let (** Do we bring the number of free variables below
		     the threshold? **)
		 val doit = ((j > 1) andalso ((m - j) <= k))

		 (** Or do we save a lot?  Note that this is NOT safe
		     for space, as dead variables might be reachable
		     from the closure. **)
		 val doit = doit orelse (j > (m div 2))

		 (** And finally, were we also over the threshold to
		     begin with? **)
		 val doit = doit andalso (m > k+1)

	     in  (** Decide whether to use the closure or not. **)
		 if doit then (difference(newfl,t),SOME v)
		 else (newfl,NONE)
	     end )
  end


in (* body of local *)      
   
(* Try to see if there are any closures already made in the environment which
 * could dramatically decrease the size of current closures.
 * shorten : lvar list -> lvar list
 *)
fun shorten([]) = []
  | shorten(fl) = (if method <> 2 then fl
	           else if length(fl) < 4 then fl
	                else retract(fl,getClosures initEnv))


(* Given a list of free variables, if k = 0 we use aggressive approach, if 
 * k = numCSregs -1, we use diligent (or lazy) approach. However when EB 
 * is false, we'll use conservative approach anyway.
 *) 
fun calleealloc(fl,k) =
  let

(***>
      val _ = COMMENT(fn () => (pr "Calleealloc:"; ilist fl))
<***)

      (* this piece of code is used to stop unsafe closure sharing used 
       * among embedied continuation functions. closlist is re-filtered
       * by the free variable list fl.
       *)
      (** Consider only closures which do not hold on to dead values.
          This version is overly restrictive: every single element
	  of the closure must be free.  If the closure has two functions,
	  both must be in the free list; in fact both will never be in
	  the free list if we have already passed the free list through
	  sameClosureOpt -- and this is always the case with the fl passed
	  to calleealloc.  A similar remark applies if one of the contents
	  is a closure. **)
      val closlist = getClosures initEnv

(***> <***)
      fun filter0 (v,CR(_,{functions,values,closures,...})) =
        ((in'(v,fl)) orelse 
	   (let val functions = map #1 functions
	        val closures = map #1 closures
	        val t1 = uniq(functions @ values @ closures)
             in foldr (fn (x,b) => (in'(x,fl) andalso b)) true t1
            end))

      val closlist = sublist filter0 closlist 
(***> <***)

      val restm = sublist (fn x => in'(x,fl)) extraArgs
      val rest = if (length restm) = numCSregs then (tl restm)
                 else restm
      exception FAIL of lvar list * int
      fun first(nil,0,res) = res
        | first(nil,k,res) = raise FAIL (res,k)
        | first(_,0,res) = res
        | first(hd::tl,k,res) = first(tl,k-1,res@[hd])
      and first0(wl,vl,k) = first(wl,k,nil) handle FAIL (c,i) => 
                               (first(vl,i,c) handle FAIL (res,_) => res)

      (** Take two lists, and return a list of length equal to the longer
          of the two lists.  The i^th element of the list will be the
          i^th element of the first list, or the i^th element of the second
	  list if the first list isn't long enough. **)
      fun punch(nil,nil) = nil
        | punch(nil,tl) = tl 
        | punch(tl,nil) = tl
        | punch(hd1::tl1,hd2::tl2) = hd1::punch(tl1,tl2)

      (* Test if it's the innermost continuation function. *)
      fun bmerge((x,xl),(y,yl)) = ((x orelse y),merge(xl,yl))

    (** the getv function may be modified by doing more data flow analysis **)
    (* Choose k lvars from vl, default is rest *)
      fun getv(vl,k,rest) = 
         if length(vl) <= k then punch(vl,rest)
         else (let val el = (map (ebinfo o (fn (v,_,_,_,_) => v)) calleeB)
                   val (EB,ncand) = foldr bmerge (false,nil) el
                in if (not EB) then  (* leaf cont nodes *)
                       punch (rest,first0(difference(vl,uniq rest),nil,k))
                   else (let val ncand = freeTmpAnalysis(ncand,initEnv) 
                             val wl = difference(vl,ncand)
                             val ul = difference(vl,wl)
                          in punch (first0(wl,ul,k),rest)
                         end)
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
                             end))
            end)
  end (* calleealloc *)         
end (* local *)

(* Decide which variables go into the closure and which variables go
   into callee-save registers. *)
val (escapeFree,rest) = 
   case (calleeB,escapeB,collected) 
    of ([],_,_) =>  (shorten(escapeFree),nil)
     | (_,[],[]) =>
         let val (floats,other) = partition isFlt calleeFree
	     val k = if fullClosure then 0 else numCSregs-1
             val (e,r) = calleealloc(other,k)
	  in (merge(floats,e),r) 
         end
     | (_,[],_) =>
	let val left = difference(calleeFree,escapeFree)
            val (floats,other) = partition isFlt left
	    val (nfl,rest) = if fullClosure
			     then calleealloc(other,0)
			     else calleealloc(other,numCSregs-1)
	in (merge(escapeFree,merge(floats,nfl)),rest)
	end
     | _ => (let val fl = merge(escapeFree,calleeFree)
                 val (floats,other) = partition isFlt fl
                 val (e,r) = calleealloc(other,0)
              in (merge(e,floats),r)
             end) 

(***>
val _ = COMMENT(fn () =>
        (pr "Free variables to be accessible from the closure:";
	 ilist escapeFree;
	 pr "Free variables to be accessible from callee-save arguments:";
	 ilist rest))
<***)

(* Given the functions to be defined in the closure (from escapeB), the free
   variables which should be contained in the closure (escapeFree), and their
   current locations (initEnv), decide on a closure representation. *)
val (closureInfo,calleeReg1) = 
      case (escapeB,escapeFree)
       of (nil,nil) => (NONE,NONE)
        | (nil,[v]) => 
             (if (isFlt v) then 
                (let val (cname,clos) = closureStrategy([],escapeFree,initEnv)
                  in (SOME clos, SOME cname)
                 end)
              else (NONE,SOME v))
        | _ => 
          (let val escapeB' = map (fn(_,v,l,_,_,_) => (v,l)) escapeB
               val (cname,clos) = closureStrategy(escapeB',escapeFree,initEnv)
            in (SOME clos, SOME cname)
           end)

val rest = case calleeReg1 of NONE => rest
                            | SOME cname => (cname::rest)

(* Add new known function information to the environment. *)
local
  fun addF(v,label,free) = (
	augment((v,Function{label=label,free=free}), baseEnv);
	())
in
val _ = app
 (case calleeReg1
    of NONE =>
	(fn{v,free,callc,label,...} =>
(*	  if callc then error "29488 in closure" 
	  else *) addF(v,label,free))
     | SOME cname =>
	(fn{v,free,callc,label,...} =>
	  if callc
	  then addF(v,label,enter(cname,free))
	  else addF(v,label,free)))
 knownB
end (* local *)

(* Final construction of the environment for each standard function. *)
val escapeFrags : (fun_kind * lvar * lvar list * cty list * cexp * env * lvar list) list =
 (case (escapeB,closureInfo)
    of (nil,_) => nil
     | (_,NONE) => error "unexpected 23422 in closure"
     | (_,SOME{cr,...}) => 
	  let val env1 = baseEnv
	      fun f ((k,v,l,args,cl,body),i) =
		let val myCname = v
		    val env = offset(i,cr,myCname,env1)
		    val (args,ncl,kl) = addExtra(args,cl)
                    val allargs = zip(args,ncl)
		    val env = foldr augmentV env allargs
(***>
		    val _ = COMMENT(fn () => (pr "\nEnvironment in escaping ";
					      vp v; pr ":\n";
					      printEnv env))
<***)
                    val cl = BOGt::BOGt::ncl

		in  inc CGoptions.escapeGen;
		    (k,l,mkLvar()::myCname::args,cl,body,env,kl)
		end
	  in  formap f escapeB
	  end)


(* Final construction of the environment for each known function. *)
val knownFrags : (fun_kind * lvar * lvar list * cty list * cexp * env * lvar list) list =
 map (fn{v,args,cl,body,free,label,callc} =>
  let fun addv(v,env) = case whatIs(initEnv,v)
             of (Function _) => error "cps/closure.223"
              | obj => augment((v,obj),env)

      val (free',env) =
        case (callc,calleeReg1)
	  of (false,_) =>
	        (inc CGoptions.knownGen;
		 (free,baseEnv))
	   | (true,NONE) => (free,baseEnv) (* error "unexpected ~4 in closure" *)
	   | (true,SOME v) =>
               (let val env' = case closureInfo
                      of NONE => addv(v,baseEnv)
                       | SOME{cname,cr,...} => 
                            augment((cname,Closure cr),baseEnv)
                 in (inc CGoptions.knownClGen; 
                     (enter(v,free),env'))
                end) 

      val env = foldr addv env free
      val (args,ncl,extra) = addExtra(args,cl)
      val allargs = zip(args,ncl)
      val env = foldr augmentV env allargs

(***>
      val _ = COMMENT(fn () => (pr "\nEnvironment in known ";
	                        vp v; pr ":\n";
				printEnv env))
<***)

      val args = args @ free'
      val cl = ncl @ (map get_cty free')

   in case extra of [] => (KNOWN,label,args,cl,body,env,extraArgs)
                  | _ => (KNOWN,label,args,cl,body,env,extra)
  end)
 knownB

(* Final construction of the environment for each callee-save
   continuation. *)
val calleeFrags = if null calleeB then []
  else
    (let val rest =
	   let (** The variables already in the calleesave arguments. **)
	       val wl = extraArgs
	       (** The new values to put in the calleesave arguments. **)
	       val wl' = uniq wl
	       val left = sublist (fn x => not (member wl' x)) rest

	       val vl = uniq rest
	       (** A first assigment of calleesave arguments -- if the
		   position is already occupied by something we need,
		   leave it there (SOME case) otherwise mark that it
		   is free to be filled (NONE case). **)
	       val base =
                 let fun g(x::tl,vl) = 
                            if member vl x then ((SOME x)::g(tl,rmv(x,vl)))
                            else (NONE::g(tl,vl))
                       | g(nil,vl) = nil
                  in g(wl,vl)
                 end
	       (** Fill in the holes in the calleesave argument template with
		   the new values.  **)
	       fun h(tl,nil) = tl
                 | h((SOME x)::tl,ul) = (SOME x)::h(tl,ul)
		 | h(NONE::tl,u::ul) = (SOME u)::h(tl,ul)
		 | h _ = error "errors in closure1.sml --- rest/h"
 	    in  h(base,left)
	   end

	 (** Only used in extramap. **)
	 fun getenv (SOME x) = 
                ((case whatIs(initEnv,x) of 
		      (Function _) => error "cps/closure437"
		    | obj => (VAR x,obj))
                 handle Lookup(tt,ee) =>
                   (case closureInfo of 
                       NONE => (raise (Lookup(tt,ee)))
                     | SOME{cname,cr,...} =>
                        (if (cname=x) then (VAR cname, Closure cr)
                         else raise (Lookup(tt,ee)))))
	   | getenv NONE = (INT 0,Value BOGt)   (*** wrong type here ***)
 
         val extramap = if numCSregs = 0 then nil
                        else (map getenv rest)

	 val _ = app (fn (v,l,_,_,_) => addinc(v,l,map #1 extramap)) calleeB

	 val extraenv = map (fn (VAR x,c) => (x,c)
                              | (_,l) => (mkLvar(),Value BOGt))
	                    extramap
         val extracty = map (fn (Value ct) => ct
                              | _ => BOGt) (map #2 extraenv)
	 val env = baseEnv
	 val env = foldr augment env extraenv

	 fun f (v,l,args,cl,body) =
	   let val allargs = zip(args,cl)
               val env = foldr augmentV env allargs
	       val nargs = mkLvar()::(map #1 extraenv)@args
               val ncl = BOGt::extracty@cl 
(***>
	       val _ = COMMENT(fn () =>
			       (pr "\nEnvironment in callee-save continuation ";
				vp v; pr ":\n";
				printEnv env))
<***)
	   in  inc CGoptions.calleeGen;
	       (CONT,l,nargs,ncl,body,env,map #1 extraenv)
	   end
      in map f calleeB
     end)

fun mkrexp(contents,cname) =
  let fun scan((VAR v,OFFp 0),(y,z)) = 
            let val t = get_cty v
                val (w,h) = wrapFltHeader(t,v)
             in ((VAR w,OFFp 0)::y,h o z)
            end
        | scan(u,(y,z)) = (u::y,z)
  
      val (contents,header) = foldr scan ([],fn ce => ce) contents

   in if not(!CGoptions.allocprof) then 
           (fn ce => header(RECORD(RK_RECORD,contents,cname,ce)))
      else let val prof =
	         case (escapeB,knownB,calleeB)
                  of (nil,_,nil) => profKClosure
	           | (nil,_,_) => profCClosure
		   | _ => profClosure
            in prof(length contents) o 
                (fn ce => header(RECORD(RK_RECORD,contents,cname,ce)))
           end
  end

val frags = escapeFrags@knownFrags@calleeFrags
val env = initEnv

val (rexp,env) =
 (case closureInfo
    of NONE => ((fn x => x),env)
     | SOME{cname,cr,contents} => 
	  let val (contents,header) =
                recordEl(map (fn v => (v, OFFp0)) contents, env)
	      val env = augment((cname,Closure cr),env)
	  in  (header o mkrexp(contents,cname),env)
	  end)

in  (* body of makenv *)
(***>
	      COMMENT(fn () => (pr "\nEnvironment after FIX:\n";
	                        printEnv env; pr "MAKENV DONE.\n\n"));
<***)
    (rexp,frags,env)
end (* makenv *)


(**********************************************************************
 * close0: MAIN LOOP of closeCPS                                      *
 **********************************************************************)
fun close0(ce,env,extraArgs) = let

fun close1(fk,f,vl,cl,ce,env,eA') =
  ((fk,f,vl,cl,close0(ce,env,eA'))
   handle Lookup(v,env) => (pr "LOOKUP FAILS on "; vp v;
			    pr "\nin environment:\n";
			    printEnv env;
			    pr "\nin function:\n";
			    PPCps.prcps ce;
			    error "Lookup failure in cps/closure.sml"))
fun close(ce,env) =
  case ce
    of FIX(bindings,b) =>
        let val (header,frags,env') = makenv(env,bindings,extraArgs)
	in  FIX(map close1 frags, header(close(b,env')))
	end
     | APP(f,args) =>
        let val obj = (case f of VAR v => whatIs(env,v) | _ => Value BOGt)
	in  case obj
	      of Closure(CR(offset,{functions,...})) =>
		   let val (env,h) = fixAccess([f],env)
		       val (args',_,h') = fixArgs(args,env)
		       val (_,label) = List.nth(functions,offset)
		       val call = APP(LABEL label,LABEL label::f::args')
		   in  if not(!CGoptions.allocprof)
		       then h(h'(call))
		       else h(h'(case args
				   of [_] => profCntkCall call
				    | _ => profStdkCall call))
		   end
	       | Function{label,free} =>
		   let (* NOTE: 0 or 1 arg will be a continuation --
			  0 if f is a known continuation. *)
		       val (args',_,header) = fixArgs(args@(map VAR free),env)
		       val call = APP(LABEL label,args')
		   in  if not(!CGoptions.allocprof)
		       then header call
		       else header(profKnownCall call)
		   end
	       | Callee(label,extra) =>
		   let val args' = extra@args
		       val (env,header) = fixAccess(label::args',env)
		       val call = APP(label,label::args')
		   in  if not(!CGoptions.allocprof)
		       then header call
		       else header(case label
			             of LABEL _ => profCSCntkCall call
	                              | _ => profCSCntCall call)
		   end
	       | Value t =>
                   let val (env,h) = fixAccess([f],env)
		       val (args',_,h') = fixArgs(args,env)
		       val l = mkLvar()
		       val call = SELECT(0,f,l,t,
                                         (APP(VAR l,(VAR l)::f::args')))
		   in  if not(!CGoptions.allocprof)
		       then h(h'(call))
		       else h(h'(profStdCall call))
		   end
	end
     | SWITCH(v,c,l) =>
	let val (env,header) = fixAccess([v],env)
	in  header (SWITCH(v,c,map (fn c => close(c,env)) l))
	end
     | RECORD(k as RK_FBLOCK,l,v,c) =>    (*** must be fresh float numbers ***)
                                          (*** accesspath must be OFFp 0 ***)
        let val (env,header) = fixAccess(map #1 l,env)
        in  header (RECORD(k,l,v,close(c,augmentV((v,BOGt),env))))
        end
     | RECORD(k,l,v,c) =>
	let val (l,header) = recordEl(l,env)
	    val record = RECORD(k,l,v,close(c,augmentV((v,BOGt),env)))
	in  if not(!CGoptions.allocprof)
	    then header record
	    else header(profRecord (length l) record)
	end
     | SELECT(i,v,w,t,c) =>
        let val (env,header) = fixAccess([v],env)
	in  header(SELECT(i,v,w,t,close(c,augmentV((w,t),env))))
	end
     | OFFSET(i,v,w,c) => error "OFFSET in cps/closure.sml!"
     | BRANCH(i,args,c,e1,e2) =>
	let val (env,header) = fixAccess(args,env)
	in  header (BRANCH(i,args,c,close(e1,env),close(e2,env)))
	end
     | SETTER(i,args,e) =>
	let val (env,header) = fixAccess(args,env)
	in  header (SETTER(i,args,close(e,env)))
	end
     | LOOKER(i,args,w,t,e) =>
	let val (env,header) = fixAccess(args,env)
	in  header (LOOKER(i,args,w,t,close(e,augmentV((w,t),env))))
	end
     | ARITH(i,args,w,t,e) =>
	let val (env,header) = fixAccess(args,env)
	in  header (ARITH(i,args,w,t,close(e,augmentV((w,t),env))))
	end
     | PURE(i,args,w,t,e) =>
	let val (env,header) = fixAccess(args,env)
	in  header (PURE(i,args,w,t,close(e,augmentV((w,t),env))))
	end
in  (* body of close0 *)
    close(ce,env)
end

val (nvl,ncl,e) = addExtra(vl,cl)

val allvl = zip(f::nvl,BOGt::ncl)
val env1 = foldr augmentV baseEnv allvl

val fe = (fk,mkLvar(),mkLvar()::f::nvl,BOGt::BOGt::ncl,close0(ce,env1,e))

in  (* body of closeCPS *)
    UnRebind.unrebind fe
end

end (* local *)

end (* functor Closure *)


(*
 * $Log: closure.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:29  george
 *   Version 109.24
 *
 *)
