(* codegen.sml *)

signature CODEGEN =
sig
  structure A : ACCESS
  structure L : LAMBDA
  structure Codenvl : CODENV
  val reOpenLookup : int -> (L.lexp * Codenvl.Env)
  val generate : L.lexp -> unit
  sharing L = Lambda and A = Access
end


functor Codegen (Machine : MACHINE) : CODEGEN =
struct

structure A : ACCESS = Access
structure L : LAMBDA = Lambda
structure Switcher : SWITCHER = Switcher(Machine)
structure Codenvl : CODENV = Codenv(Machine)
open Codenvl Basics Opt L

structure ReOpenTable =
   Dynamic(struct open Array
		  type elem = (L.lexp * Codenvl.Env)
		  type array = elem array
           end)

val reOpenTable = ReOpenTable.array(RECORD[],Codenvl.env0)
local val reOpenCount = ref 0
      open ReOpenTable
in   fun addToTable x = 
	(inc reOpenCount; update(reOpenTable,!reOpenCount,x);
	 !reOpenCount)
     fun reOpenLookup i = reOpenTable sub i
end
				       
structure Prof =
  struct
    (* these values are taken from runtime/prof.h *)
    val links = 6
    val linkslots = 256
    val biglinks = (links+linkslots)
    val closures = (biglinks+1)
    val closureslots = 256
    val bigclosures = (closures+closureslots)
    val records = (bigclosures+1)
    val recordslots = 5
    val bigrecords = (records+recordslots)

    fun closureaccess i =
	    if !CGoptions.profile
	      then if i>=linkslots then (Machine.profile(links,1);
					   Machine.profile(biglinks,i))
				   else Machine.profile(links+i,1)
	      else ()
    fun closure i =
	    if !CGoptions.profile
		then if i>=closureslots then (Machine.profile(closures,1);
					      Machine.profile(bigclosures,i))
					else Machine.profile(closures+i,1)
		else ()
    fun record i =
	    if !CGoptions.profile
	      then if i>=recordslots then (Machine.profile(records,1);
					   Machine.profile(bigrecords,i))
				     else Machine.profile(records+i,1)
	      else ()
  end (* structure Prof *)

type Label = Machine.Label
val newlabel = Machine.newlabel

val offset = Machine.offset

val fragments = ref(nil: (Label * L.lexp * Codenvl.Env) list)
fun newfragment frag = fragments := frag::(!fragments)

val rec simple =
      fn PRIM _ => false
       | VAR _ => true
       | SELECT(_,e) => simple e
       | RECORD nil => true
       | INT _ => true
       | REAL _ => true
       | STRING _ => true
       | _ => false 
    
fun generate(e : lexp) =
let
val (env0,makenv) = Codenvl.codenv e
fun codegen(lab : Label, e0 as FN(v,e) : lexp, env: Env) =
 let fun genfn env =
   let fun gen(e : lexp, last : bool) : unit =
     let val access =
		  fn PATH (p as 0::_::_::_) =>
			(Prof.closureaccess(length p - 2); Machine.path p)
		   | PATH p => Machine.path p
		   | KNOWN lab => Machine.label lab
		   | CONST i => Machine.const i
	 fun copy(i,acc::r) = (copy(i+1,r); access acc; Machine.nextelem i)
	   | copy(n,nil) = ()
     in case e of VAR w => access (lookup env w)
	| APP(FN(w,b), VAR x) =>
		genfn (augment(env,(w, lookup env x))) (b,last)
	| APP(FN(w,b), INT i) =>
		genfn (augment(env,(w, CONST i))) (b,last)
	| APP(FN(w,b), e as STRING s) =>
		if length s = 1
		 then genfn (augment(env,(w, CONST(ord s)))) (b,last)
		 else let val lab = newlabel()
		      in  newfragment(lab,e,env);
			  genfn (augment(env,(w, KNOWN lab))) (b,last)
		      end
	| APP(FN(w,b), e as REAL _) =>
		let val lab = newlabel()
		in  newfragment(lab,e,env);
		    genfn (augment(env,(w, KNOWN lab))) (b,last)
		end
	| APP(FN(w,b),FN _) =>
		let val (env',frag,close) = makenv(e, env, ~(!offset)-1)
		    val len = length close
		in  Prof.closure len;
		    Machine.startalloc len;
		    copy(0, close);
		    Machine.endclosure();
		    app newfragment frag;
		    genfn env' (b, last);
		    Machine.squeeze()
		end
	| APP(FN(w,b),a) =>
		(gen(a,false);
		 genfn (augment(env,(w, PATH[~(!offset),0]))) (b,last);
		 Machine.squeeze())
	| APP(f as PRIM i, a) =>
		(case (Machine.canapp i, a)
		   of (1,_) => (gen(a,false); Machine.primapp i)
		    | (_,RECORD l) => (app (fn b => gen(b,false)) l;
				       Machine.primapp i)
		    | (n,_) =>
			let val v = A.mkLvar()
			    fun ith 0 = nil
			      | ith i = SELECT(n-i,VAR v)::ith(i-1)
			in  gen(APP(FN(v,APP(f,RECORD(ith n))),a),
			        last)
			end)
	| APP(f as VAR w, a) => 
	       (case (last, lookup env w, !CGoptions.tailrecur, !CGoptions.tail)
		 of (true, PATH[0,i], true, _) => (gen(a,false);
						   Machine.tailrecur i)
		  | (true, _, _, true) => (gen(f,false); gen(a,false);
					   Machine.tail())
		  | _ => (gen(f,false); gen(a,false); Machine.apply()))
	| APP(f,a) => (gen(f,false); gen(a,false);
		       if last andalso !CGoptions.tail
			then Machine.tail()
			else Machine.apply())
	| FN(v,n) =>
		let val (_,frag,close) = makenv(e, env, 0)
		    val len = length close
		in  Prof.closure len;
		    Machine.startalloc len;
		    copy(0, close);
		    Machine.endclosure();
		    app newfragment frag
		end
	| FIX(nil,_,b) => gen(b,last)
        | FIX(vl as (v::_), el,b) => 
		let val (env',frags,close) = makenv(e, env, ~(!offset)-1)
		    val len = length close
		in  Prof.closure len;
		    Machine.startalloc len;
		    copy(0, close);
		    Machine.endclosure();
		    app newfragment frags;
		    genfn env' (b, last);
		    Machine.squeeze()
		end
	| INT i => Machine.const i
	| STRING s =>
		if length s = 1 then Machine.const(ord s)
		else let val lab = newlabel()
		     in  Machine.label lab;
			 newfragment(lab,e,env)
		     end
	| REAL s =>
		let val lab = newlabel()
		in  Machine.label lab;
		    newfragment(lab,e,env)
		end
	| SWITCH (x as (APP(PRIM i, RECORD[y,z]),
		        [(DATAcon(DATACON{rep=(CONSTANT c1),...}),e1),
		         (DATAcon(DATACON{rep=(CONSTANT c2),...}),e2)],
		        NONE)) =>
	   let val skip = newlabel() and join = newlabel()
	       fun g(f,tr,fa) = (gen(y,false); gen(z,false);
			         f skip; gen(fa,false); 
				 Machine.endcase(join,skip);
				 gen(tr,last); Machine.endswitch join)
	    in case (Machine.tester i, c1, c2)
	     of (SOME f, 0, 1) => g(f,e2,e1)
	      | (SOME f, 1, 0) => g(f,e1,e2)
	      | _ =>  let fun makecon con = let val lab = newlabel()
					    in  newfragment(lab,con,env);
					        lab
					    end
		      in  Switcher.genswitch (gen,last,makecon,x)
		      end
	   end
	| SWITCH x => let fun makecon con = let val lab = newlabel()
					    in  newfragment(lab,con,env);
					        lab
					    end
		      in  Switcher.genswitch (gen,last,makecon,x)
		      end
	| RECORD nil => Machine.const 0
	| RECORD l =>
		let fun elem(i,e::r) = 
				(if simple e
				    then (elem(i+1,r); gen(e,false))
				    else (gen(e,false); elem(i+1,r));
				 Machine.nextelem i)
		      | elem (n,nil) = (Prof.record n; Machine.startalloc n)
		in  elem (0,l);
		    Machine.endrecord()
		end
	| PRIM i =>
		let val v = A.mkLvar()
		    fun ith 0 = nil
		      | ith i = ith(i-1)@[SELECT(i-1,VAR v)]
		in  case Machine.canapp i
		      of 1 => gen(FN(v, APP(e,VAR v)), last)
		       | n => gen(FN(v,APP(e,RECORD(ith n))), last)
		end
	| SELECT(i,a) => (gen(a,false); Machine.select i)
	| HANDLE(a,h) =>
		let val labels = (newlabel(), newlabel())
		    val v = A.mkLvar()
		in  Machine.starthandle(labels);
		    gen(a,false);
		    Machine.midhandle(labels);
		    genfn (augment(env,(v,PATH[~(!offset),0])))
			  (APP(h,VAR v),false);
		    Machine.squeeze();
		    Machine.endhandle(labels)
		end
	| RAISE e => (gen(e,false); Machine.raisexn())
       end (* fun gen *)
   in  gen
   end (* fun genfn *)
 in  Machine.definelabel(lab,
	if !System.Control.reopen
	    then addToTable(e0,env) else 0);
     offset := 2;
     genfn(augment(env,(v,PATH[~1,0]))) (e,true);
     Machine.return()
 end
  | codegen(lab,STRING s,_) = Machine.stringconst(s,lab)
  | codegen(lab,REAL r,_) = Machine.realconst(r,lab)
  | codegen _ = ErrorMsg.impossible "codegen.codegen"

in  fragments := [(newlabel(),e,env0)];
    while length(!fragments) <> 0 (* temporary hack, compiler bug *)
     do let val (frag::rest) = !fragments
	in  fragments := rest;
	    codegen frag
	end
end

end  (* functor Codegen *)
