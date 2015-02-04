(* codenv.sml *)

signature CODENV =
  sig
    structure L : LAMBDA sharing L=Lambda
    structure A : ACCESS sharing A=Access
    exception Notfound_Codenv of A.lvar
    type Env
    type Entry
    type Label
    datatype Access = KNOWN of Label
		    | PATH of int list
		    | CONST of int
    val VARentry : A.lvar -> Entry
    val lookup : Env * A.lvar -> Access
    val augment : Env * (Entry * Access) -> Env
    val codenv : L.lexp -> Env * 
			   (L.lexp * Env * int
		 		-> Env * (Label * L.lexp * Env) list * Access list)
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

datatype Entry = VARentry of lvar | ENVentry of (Entry * Access) list

type Env = (Entry*Access) list

val lvarName = A.lvarName

exception Root

fun root [r] = r
  | root (_::tl) = root tl
  | root nil = raise Root

fun sublist test =
    let fun subl(a::r) = if test a then a::(subl r) else subl r
          | subl nil = nil
    in  subl
    end

exception Compose

fun composepath ([i:int],j::rest) = (i+j)::rest
  | composepath (i::a, l) = i::(composepath(a,l))
  | composepath _ = raise Compose

fun compose(PATH a, PATH b) = PATH(composepath(a,b))
  | compose(a, b as KNOWN _ ) = b
  | compose(a, b as CONST _ ) = b
  | compose _ = raise Compose

fun augment(env,pair) = pair::env

fun transform(nil,acc) = nil
  | transform(env,acc) = [(ENVentry env, acc)]

(* env printing stuff *)
fun printacc (KNOWN _) = (print " : Known\n"; ())
  | printacc (CONST i) = (print " : Const "; print i; newline())
  | printacc (PATH l) =
	(print " : Path ";
	 printClosedSequence ("[",",","]") (fn i => (Integer.print i; ())) l;
	 newline())
fun flatPr(vl,free,close) =
    let fun pr (nil,nil) = ()
	  | pr (v::vl,acc::accl) = (print (lvarName v); printacc acc; pr(vl,accl))
	fun fpr (nil,free,close) = (newline(); pr (free,close))
	  | fpr (v::vl,free,_::close) = (print (" " ^ lvarName v);
					 fpr(vl,free,close))
    in  if !CGoptions.closureprint
	  then (print "\nClosure:"; fpr(vl,free,close))
	  else ()
    end
fun purelinkPr(vl,free,close,NONE) = flatPr(vl,free,close)
  | purelinkPr(vl,free,close,SOME(e,_)) =
    let fun prLink (ENVentry nil) = (print "BOGUS"; ())
	  | prLink (ENVentry((e,_)::_)) = (print "link to "; prLink e)
	  | prLink (VARentry v) = (print ("closure of " ^ lvarName v); ())
	fun pr (_,nil) = ()
	  | pr (nil,acc::_) = (prLink e; printacc acc)
	  | pr (v::vl,acc::accl) = (print (lvarName v); printacc acc; pr(vl,accl))
	fun fpr (nil,free,close) = (newline(); pr (free,close))
	  | fpr (v::vl,free,_::close) = (print (" " ^ lvarName v);
					 fpr(vl,free,close))
    in  if !CGoptions.closureprint
	  then (print "\nClosure:"; fpr(vl,free,close))
	  else ()
    end

exception Notfound_Codenv of A.lvar
fun lookup (env,w) =
    let fun get ((VARentry v, acc)::r) = if v=w then acc else get r
	  | get ((ENVentry e, acc)::r) =
		(get r handle Notfound_Codenv _ =>
			(compose(acc, lookup(e,w))
				handle Compose => raise Notfound_Codenv w))
          | get nil = raise Notfound_Codenv w
    in  get env 
    end

fun flat freevars (e as FN(v,b), env, _) =
     let val lf = newlabel()
	 val free = freevars v
			handle Intmap.Intmap => nil (* for inline functions *)
	 val close = KNOWN lf :: map (fn v => lookup(env,v)) free
	 fun f(v::vl, i) = (VARentry v, PATH[0,i,0]) :: f(vl,i+1)
	   | f(nil,_) = nil
	 val env' = f(free,1)
     in  flatPr([v],free,close);
	 (nil, [(lf,e,env')], close)
     end
  | flat freevars (FIX(vl as v::_, el, b), env, offset) =
     let val free = freevars v
	 fun f(v::vl, i) = (VARentry v, PATH[i,0]) :: f(vl,i+1)
	   | f(nil,_) = nil
	 fun g(v::vl, i) = (VARentry v, PATH[i]) :: g(vl,i+1)
	   | g(nil,i) = f(free, i)
	 val env' = g(vl, 0) 
	 fun frags(e::re, i) = 
		(newlabel(), e, transform(env',PATH[0,~i]))::frags(re,i+1)
	   | frags(nil,_) = nil
	 val fgs = frags(el,0)
	 val close = map (KNOWN o #1) fgs @ map (fn v => lookup(env,v)) free
     in  flatPr(vl,free,close);
	 ((ENVentry env',PATH[offset,0])::env, fgs, close)
     end

fun purelink freevars (e as FN(v,b), env, _) =
     let val lf = newlabel()
	 val free = freevars v
			handle Intmap.Intmap => nil (* for inline functions *)
	 val link = (case root env of l as (ENVentry _,_) => SOME l | _ => NONE)
			handle Root => NONE
	 val newfree =
		case link
		  of SOME l => sublist (fn v => (lookup([l],v); false)
					handle Notfound_Codenv _ => true) free
	 	   | NONE => free
	 val close = KNOWN lf :: map (fn v => lookup(env,v)) newfree
			@ (case link of NONE => nil | SOME (_,acc) => [acc])
	 fun f(v::vl, i) = (VARentry v, PATH[i,0]) :: f(vl,i+1)
	   | f(nil,i) = (case link of NONE => nil | SOME (e,_) => [(e,PATH[i,0])])
	 val env' = [(ENVentry(f(newfree,1)),PATH[0,0])]
     in  purelinkPr([v],newfree,close,link);
	 (nil, [(lf,e,env')], close)
     end
  | purelink freevars (FIX(vl as v::_, el, b), env, offset) =
     let val free = freevars v
	 val link = (case root env of l as (ENVentry _,_) => SOME l | _ => NONE)
			handle Root => NONE
	 val newfree =
		case link
		  of SOME l => sublist (fn v => (lookup([l],v); false)
					handle Notfound_Codenv _ => true) free
	 	   | NONE => free
	 fun f(v::vl, i) = (VARentry v, PATH[i,0]) :: f(vl,i+1)
	   | f(nil,i) = (case link of NONE => nil | SOME (e,_) => [(e,PATH[i,0])])
	 fun g(v::vl, i) = (VARentry v, PATH[i]) :: g(vl,i+1)
	   | g(nil,i) = f(newfree, i)
	 val env' = g(vl, 0) 
	 fun frags(e::re, i) = 
		(newlabel(), e, transform(env',PATH[0,~i]))::frags(re,i+1)
	   | frags(nil,_) = nil
	 val fgs = frags(el,0)
	 val close = map (KNOWN o #1) fgs @ map (fn v => lookup(env,v)) newfree
			@ (case link of NONE => nil | SOME (_,acc) => [acc])
     in  purelinkPr(vl,free,close,link);
	 ((ENVentry env',PATH[offset,0])::env, fgs, close)
     end

fun codenv e =
	if !CGoptions.chained then (nil, purelink (Opt.free e))
	else (nil, flat (Opt.free e))
end (* functor Codenv *)
