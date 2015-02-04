(* codenv.sml *)

signature CODENV =
 sig
        structure L : sig type lexp end
        structure V : sig type lvar end
        type Label
	datatype Access = KNOWN of Label | CONST of int
			| KNOWNFUNC of Label * Access
			| PATH of int list
	datatype Entry = VARentry of V.lvar | ENVentry of (Entry * Access) list
        type Env
	exceptionx notfound_Codenv : V.lvar
	val lookup : Env * V.lvar -> Access
	val augment : Env * (Entry * Access) -> Env
        val makenv : V.lvar list * L.lexp list * int * Env -> Env * Access list
        val transform : Env * Access -> Env
	val printclose : V.lvar list -> Env -> unit
end

functor Codenv(Label : sig type Label end) : CODENV =
    
struct

structure L : LAMBDA = Lambda
structure V : sig type lvar end = struct type lvar = int end

open Basics Label L V

datatype Access = KNOWN of Label | CONST of int
		| KNOWNFUNC of Label * Access
		| PATH of int list

datatype Entry = VARentry of lvar | ENVentry of (Entry * Access) list

type Env = (Entry*Access) list;

val lvarName = Access.lvarName

(* env printing stuff *)
fun intlist l =
	let fun printlist nil = ()
	      | printlist ((i:int)::nil) = (print i; ())
	      | printlist (i::more) = (print i; print ","; printlist more)
	in print "[";
	   printlist l;
	   print "]"
	end
val margin = ref 0;
val shift = 4
fun dent() =
	let fun dodent 0 = ()
	      | dodent n = (print " "; dodent(n-1))
	in
		dodent (!margin)
	end
fun indent() = margin := !margin + shift
fun undent() = margin := !margin - shift
fun printacc (KNOWN l) = (print " : Known\n"; ())
  | printacc (CONST i) = (print " : Const "; print  i; print "\n"; ())
  | printacc (KNOWNFUNC (l,acc)) =
	(print " : KnownFunc(";
	 printacc acc;
	 print ")\n"; ())
  | printacc (PATH l) = (print " : Path "; intlist l; print "\n"; ())
fun envPrint (VARentry lvar,acc) =
		(dent();
		 print (lvarName lvar);
		 printacc acc)
  | envPrint (ENVentry el,acc) =
		(dent();
		 print "New env";
		 printacc acc;
		 indent();
		 app envPrint el;
		 undent())
fun printclose vl el =
	let fun printfunc lvar = (print " "; print (lvarName lvar); ())
	in
	   print "\nClosure contents:\n";
	   print "Functions:";
	   app printfunc vl;
	   print "\n";
	   print "Values:\n";
	   app envPrint el
	end

exceptionx notfound_Codenv : lvar;

exceptionx compose

fun composepath ([i:int],j::rest) = (i+j)::rest
  | composepath (i::a, l) = i::(composepath(a,l))
  | composepath _ = raisex compose

fun compose(PATH a, PATH b) = PATH(composepath(a,b))
  | compose(a, KNOWNFUNC(lab, b)) = KNOWNFUNC(lab, compose(a,b))
  | compose(a, b as CONST _ ) = b
  | compose(a, b as KNOWN _ ) = b
  | compose _ = raisex compose

fun lookup (env,w) =
    let fun get ((VARentry v, acc)::r) = if v=w then acc else get r
	  | get ((ENVentry e, acc)::r) =
		compose(acc, lookup(e,w))
	        handlex compose => get r
		    || notfound_Codenv => get r
          | get nil = raisex notfound_Codenv with w
     in get env 
    end;

fun augment(env,pair) = pair::env

fun root [v] = v | root (_::p) = root p
  | root _ = ErrorMsg.impossible "34 in codenv";

fun makenv(vl,bl,offset,env) =
   let val free = ref nil;
       val alists = ref nil;
       val count = ref offset;
       fun append (l,x) = l := x :: (!l)
       fun makb bound =
        let fun mak e = 
	  case e
	  of VAR w => if exists ((fn v => v=w), bound)
		       orelse ((lookup(!free, w); true) handlex notfound_Codenv=>false)
			then ()
		        else (case lookup(env,w)
			  of b as KNOWN _ => append(free,(VARentry w, b))
			   | KNOWNFUNC(lab,acc) => 
				  (append(free,(VARentry w,
					    KNOWNFUNC(lab, PATH[!count,0])));
				   append(alists, acc);
				   inc count)
			   | a => (append(free,(VARentry w, PATH[!count,0]));
				   append(alists,a);
				   inc count))
	   | FN (w,b) => makb (w::bound) b
	   | FIX (vl,el,b) => 
	       app (makb (vl @ bound)) (b::el)
	   | APP (f,a) => (mak f; mak a)
	   | SWITCH(e,l,d) =>
		let fun rootp [x] = x | rootp (a::b) = rootp b
		    fun f (DATAcon(DATACON{rep=ref(VARIABLE(Access.PATH p)),...}), e) =
			    (mak(VAR(rootp p)); mak e)
		      | f (c,e) = mak e
		  in (mak e; app f l;
		     case d of NONE => ()
			     | SOME a => mak a)
		 end
	   | RECORD l => app mak l
	   | SELECT (i,e) => mak e
	   | HANDLE (a,h) => (mak a; mak h)
	   | RAISE e => mak e
	   | INT _ => () | STRING _ => () | REAL _ => ()
	 in mak
	end
    in if !CGoptions.chained
	then app (fn (ENVentry e, acc) => 
			(append(free,(ENVentry e,PATH[!count,0]));
				        append(alists, acc);
					inc count)
		   | _ => ()) env
	else ();
       app (makb(0::vl)) bl;   (* v0 can never appear in an environment *)
       (!free, rev(!alists))
   end

fun transform(env,acc) = [(ENVentry env, acc)]

end
