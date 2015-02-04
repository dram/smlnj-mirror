signature codegen =
sig
  structure Access : ACCESS
  structure Lambda : LAMBDA
  exceptionx notfound : Access.lvar
  val generate : Lambda.lexp -> unit
end;


functor codegen (machine : machine) : codegen =
struct

structure Access = Access
structure Lambda = Lambda

val offset = machine.offset
structure Codenvl = Codenv(machine)
open Codenvl

open Basics Opt Lambda

type Label = machine.Label
val newlabel = machine.newlabel

val fragments = ref nil;

fun newfragment frag = fragments := frag::(!fragments);

val rec simple =
      fn VAR 0 => false
       | VAR _ => true
       | SELECT(_,e) => simple e
       | RECORD nil => true
       | INT _ => true
       | REAL _ => true
       | STRING _ => true
       | _ => false     


fun translatepath [v] = VAR v
  | translatepath (x::p) = SELECT(x,translatepath p)
  | translatepath _ = ErrorMsg.Impossible "1356 in codegen"

structure prof =
 struct
	val LINKS = 6
	val LINKSLOTS = 256
	val BIGLINKS = (LINKS+LINKSLOTS)
	val CLOSURES = (BIGLINKS+1)
	val CLOSURESLOTS = 256
	val BIGCLOSURES = (CLOSURES+CLOSURESLOTS)
	val RECORDS = (BIGCLOSURES+1)
	val RECORDSLOTS = 5
	val BIGRECORDS = (RECORDS+RECORDSLOTS)

        fun closureaccess i =
	     if !CGoptions.closurecount
		then if i>=LINKSLOTS then (machine.profile(LINKS,1);
					   machine.profile(BIGLINKS,i))
				     else machine.profile(LINKS+i,1)
	        else ()
        fun makeclosure (vl,env,i) =
	     (if !CGoptions.closurecount
		then if i>=CLOSURESLOTS then (machine.profile(CLOSURES,1);
					      machine.profile(BIGCLOSURES,i))
					else machine.profile(CLOSURES+i,1)
	        else ();
	      if !CGoptions.closureprint then Codenvl.printclose vl env
					 else ())
        fun makerecord i =
	     if !CGoptions.closurecount
		then if i>=RECORDSLOTS then (machine.profile(RECORDS,1);
					     machine.profile(BIGRECORDS,i))
				       else machine.profile(RECORDS+i,1)
	        else ()
end

structure Switcher = 
struct 
  open Basics Lambda

  val newlabel = machine.newlabel

fun translatepath [v] = VAR v
  | translatepath (x::p) = SELECT(x,translatepath p)
  | translatepath _ = ErrorMsg.Impossible "1356 in switch"

  fun sublist test =
    let fun subl(a::r) = if test a then a::(subl r) else subl r
          | subl nil = nil
     in subl
    end

  fun count test =
    let fun subl(a::r) = if test a then 1+(subl r) else subl r
          | subl nil = 0
     in subl
    end

  fun fst(a,b) = a

  fun isboxed (DATAcon(DATACON{rep=ref(CONSTANT _),...})) = false
    | isboxed (DATAcon(DATACON{rep=ref TRANSU,...})) = false
    | isboxed (DATAcon _) = true
    | isboxed (REALcon _) = true
    | isboxed (STRINGcon s) = (length s <> 1)
    | isboxed _ = false

  fun genswitch (gen,last,makecon,(a,l,default)) =
    let val join = newlabel()
	fun gencase (dc, b:lexp) =
	    let val skip = newlabel()
	     in case dc
		 of DATAcon(DATACON{rep=ref(TAGGED number),...}) =>
			machine.testcase_int(number,skip)
		  | DATAcon(DATACON{rep=ref(CONSTANT number),...}) =>
			machine.testcase_int(number,skip)
		  | INTcon i =>
			machine.testcase_int(i,skip)
		  | DATAcon(DATACON{rep=ref(VARIABLE(Access.PATH p)),...}) => 
			(gen(translatepath p,false);
			 machine.testcase(skip))
		  | STRINGcon s =>
		     if length(s)=1
		      then machine.testcase_int(ord(s),skip)
		      else machine.testcase_string(makecon(STRING s),
						   skip, length s)
		  | REALcon s =>
		      machine.testcase_real(makecon(REAL s),skip)
		  | _ => ();
		gen(b,last);
		machine.endcase(join,skip)
	    end
	fun gencases (l as (_,e)::r, n) =
		if length l = n
		    then (app gencase r; 
			  gen(e,last); machine.endcase(join,newlabel()))
		    else app gencase l
	  | gencases _ = ()
	val infinity = 10000000
	val (BinT,UinT) = 
	    case l
	     of (DATAcon(DATACON{dcons= ref dcons,...}),_)::_ =>
		    (count (isboxed o DATAcon) dcons, 
		     count (not o isboxed o DATAcon) dcons)
	      | (DATAcon d,_)::_ => (* exception constructor, most likely *)
			(infinity,0)
	      | (INTcon _,_) :: _ => (0,infinity)
	      | (REALcon _,_) :: _ => (infinity,0)
	      | (STRINGcon _,_) :: _ => (infinity, infinity)
	      |  nil => ErrorMsg.Impossible "103 in codegen"
	val B = sublist (isboxed o fst) l
	val U = sublist (not o isboxed o fst) l
     in gen(a,false);
	machine.startswitch();
	case (B,BinT, U, UinT)
	 of (nil, 0, U, n) =>  gencases(U,n)
	  | (B as (REALcon _,_)::_, n, nil, 0) => gencases(B,n)
	  | (B, n, nil, 0) => (machine.gettag(); gencases(B,n))
	  | (B as (STRINGcon _, _)::_, nb, U, nu) =>
	            let val boxl = newlabel()
			and defl = newlabel()
		     in machine.testboxed boxl;
			gencases(U,nu);
			machine.boxed (boxl,defl);
			gencases(B,nb);
			machine.endboxed defl
		    end
	  | (B,nb,U,nu) =>let val boxl = newlabel()
			and defl = newlabel()
		     in machine.testboxed boxl;
			gencases(U,nu);
			machine.boxed (boxl,defl);
			machine.gettag();
			gencases(B,nb);
			machine.endboxed defl
		    end;
	case default of NONE => machine.const 0  (* to get offset *)
	        | SOME e => gen(e,last);
	machine.endswitch(join)
    end
end

fun codegen( L : Label, FN(v,e) : lexp, env: Env) =
 let fun genfn (env) =
   let fun gen(e : lexp, last : bool) : unit =
	let val access =
		  fn PATH (p as (0::_::_)) =>(prof.closureaccess(length p -1);
					    machine.path p)
		   | PATH p => machine.path p
		   | KNOWN lab => machine.label lab
		   | CONST i => machine.const i
		   | _ => ErrorMsg.Impossible "208 in codegen"
	    fun copy(i,acc::r) = (copy(i+1,r); access acc; machine.nextelem i)
	      | copy(n,nil) = ()
	 in case e
	  of VAR w => access (lookup(env,w))
	  | APP(FN(w,b), VAR x) =>
		    genfn (augment(env,(VARentry w, lookup(env,x)))) (b,last)
	  | APP(FN(w,b),a) =>
		    (let val true = !CGoptions.knownfunc
			 val FN(u,c) = a
			 val true = applyOnly(w,b)
			 val lf = newlabel()
			 val env2 = augment(env,(VARentry w, KNOWNFUNC(lf,PATH[0])))
			 val env3 = transform(env2,PATH[0,0])
		      in newfragment(lf,a,env3);
			 genfn env2 (b, last)
		     end
		    handlex varstruct =>
		    (gen(a,false);
		     genfn (augment(env,(VARentry w, PATH[~(!offset),0]))) (b,last);
		     machine.squeeze()))
	| APP (f,a) => (let val true = !CGoptions.tailrecur
			    val true = last;
			    val VAR w = f;
			    val PATH [0,i] = lookup(env,w)
			 in gen(a,false);
			    machine.tailrecur(i)
			end
		       handlex bind =>
		       (let val VAR w = f
			    val KNOWNFUNC(lab, acc) = lookup(env,w)
			 in access acc;
			    gen(a,false);
			    machine.applyknown(lab)
		        end
		       handlex bind =>
		       (let val true = !CGoptions.primapp
			    val SELECT(i,VAR 0) = f
			    val n = machine.canapp i
			  in case (n,a)
			      of (1,_) => (gen(a,false); machine.primapp i)
			       | (_,RECORD l) =>
				     (app (fn b => gen(b,false)) l;
				      machine.primapp i)
			       | (_,_) =>
				    let val v = Access.mkLvar()
				        fun ith 0 = nil
					  | ith i = SELECT(n-i,VAR v)::ith(i-1)
				     in gen(APP(FN(v,APP(f,RECORD(ith n))),a),
					    last)
			            end
			end
		       handlex bind =>
		       (gen(f,false);
			gen(a,false);
			if last andalso !CGoptions.tail
				then machine.tail()
				else machine.apply()))))
	  | FN (v,b) => let val (env1,close) = makenv ([v],[b],1,env) 
		        val lf = newlabel()
		     in prof.makeclosure ([v],env1,1+length close);
			machine.startalloc(1 + length(close));
			copy(0, (KNOWN lf)::close);
			machine.endclosure();
			newfragment(lf,e,transform(env1,PATH[0,0]))
		    end
        | FIX (vl,el,b) => 
		    let val ll = map (fn _ => newlabel()) el;
		        val N = length vl;
		        val (env1,close) = makenv(vl, el, N, env);
			val M = length close;
			fun sib(w::tl,i,e) = augment(sib(tl,i+1,e),
							(VARentry w, PATH[i]))
			  | sib(nil,n,e) = e
			val env2 = sib(vl,0,env1)
		        fun frags (lab::rl, e::re, i) =
		    	    (newfragment(lab,e,transform(env2,PATH[0,~i]));
			     frags(rl,re,i+1))
		          | frags (nil,nil,i) = ()
			  | frags _ = ErrorMsg.Impossible "12 in codegen"
		     in prof.makeclosure(vl,env1,N+M);
			machine.startalloc(N+M);
			copy(0, (map KNOWN ll)@close);
		        machine.endclosure();
			frags(ll,el,0);
			genfn(augment(env,(ENVentry env2, PATH[~(!offset),0])))
			     (b,last);
			machine.squeeze()
		    end
	| INT i => machine.const i
	| STRING s =>
		    if length s = 1 then machine.const(ord(s))
		    else let val lab = newlabel()
		     in machine.label lab;
			newfragment(lab,e,env)
		    end
	| REAL s =>
		    let val lab = newlabel()
		     in machine.label lab;
			newfragment(lab,e,env)
		    end
	| SWITCH x => let fun makecon con = let val lab = newlabel()
					     in newfragment(lab,con,env);
					        lab
					    end
		       in Switcher.genswitch (gen,last,makecon,x)
		      end
	| RECORD nil => machine.const 0
	| RECORD l => let fun elem (i, e::r) = 
				(if simple e
				    then (elem(i+1,r); gen(e,false))
				    else (gen(e,false); elem(i+1,r));
				 machine.nextelem(i))
			    | elem (n, nil) = (prof.makerecord n;
					       machine.startalloc n)
		      in elem (0,l);
			 machine.endrecord()
		     end
	| SELECT (i, VAR 0) =>
		     let val v = Access.mkLvar()
	                 fun ith 0 = nil
		           | ith i = ith(i-1)@[SELECT(i-1,VAR v)]
		      in case machine.canapp i
			 of 1 => gen(FN(v, APP(e,VAR v)), last)
	                  | n => gen(FN(v,APP(e,RECORD(ith n))),last)
		     end
	| SELECT (i,a) =>
(* let exceptionx selectpath
			      val rec selectpath =
			         fn VAR 0 => raisex selectpath
			         | VAR w => (case lookup(env,w)
					    of PATH p => p
					     | _ => raisex selectpath)
			         | SELECT(i,e) => (selectpath e) @ [i]
			         | _ => raisex selectpath
			  in machine.path (selectpath e)
			     handlex selectpath =>
*)
			     (gen(a,false);
			      machine.select(i))
(*		         end   *)
	| HANDLE (a,h) => let val labels = (newlabel(), newlabel())
			      val v = Access.mkLvar()
			   in machine.starthandle(labels);
			      gen(a,false);
			      machine.midhandle(labels);
			      genfn(augment(env,(VARentry v,PATH[~(!offset),0])))
					(APP(h,VAR v),false);
			      machine.squeeze();
			      machine.endhandle(labels)
			  end
	| RAISE e => (gen(e,false); machine.raisexn())
	| _ => ErrorMsg.Impossible "4848 in codegen"
       end
    in gen
   end
  in machine.definelabel(L);
     offset := 2;
     genfn (augment(env,(VARentry v,PATH[~1,0]))) (e,true);
     machine.return()
 end
  | codegen(L,STRING s,_) = machine.stringconst(s,L)
  | codegen(L,REAL r,_) = machine.realconst(r,L)
  | codegen _ = ErrorMsg.Impossible "83785 in codegen"

fun generate(e : lexp) =
   (fragments := [(newlabel(),e,nil)];
    while length(!fragments) <> 0  (*temporary hack, compiler bug *)
     do let val (frag::rest) = !fragments
	 in fragments := rest;
	    codegen frag
	end
   );
end;
