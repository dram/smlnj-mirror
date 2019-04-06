signature BATCH =
sig
  val compile : string -> unit
  val bootEnv : bool -> int list
end


functor Batch(structure CM : CMACHINE
	      val start : string -> unit
	      val stop : unit -> unit) : BATCH =
struct

structure CPSg = CPSgen(CM)
structure CPSopt = CPSopt(val maxfree = 3+length(CM.miscregs))
structure Closure = Closure(val maxfree = 3+length(CM.miscregs))
structure Spill = Spill(val maxfree = 3+length(CM.miscregs))

open ErrorMsg Access Basics BareAbsyn PreBatch

fun compile fname =
  let fun a (lexp,s) =
	let val _ = start s
	    val write = CM.comment
	    val _ = debugmsg "convert"
	    val (function,ctable) = Convert.convert lexp
	    fun fprint (function : CPS.function) =
	      (if !CPSoption.printit
		then CPSprint.show write (Intmap.map ctable)
				(CPS.FIX([function],CPS.PRIMOP(0,[],[],[])))
		else ())
	in  write "After convert:\n"; fprint function;
	    if !CPSoption.printsize
		then case function of (_,_,ce) => CPSsize.printsize ce else ();
            (function,write,fprint,ctable)
        end
      fun b ((f,vl,cps),write,fprint,ctable) =
	let val _ = debugmsg "cpsopt"
	    val cps = CPSopt.reduce ctable cps
	    val function = (f,vl,cps)
	    fun newconst c =
		 let val v = mkLvar()
		 in  Intmap.add ctable (v,CPS.INTconst c); v
		 end
	    fun prof(a,b,ce) = CPS.PRIMOP(Profile.profPrim,
				    [newconst a,newconst b],nil,[ce])
	    val ctable = Intmap.map ctable
	    val constant = fn w => ((ctable w; true)
						handle Ctable => false)
        in  write "\nAfter cpsopt:\n"; fprint function;
	    if !CPSoption.printsize
		then case function of (_,_,ce) => CPSsize.printsize ce else ();
	    (function,write,fprint,ctable,constant,prof)
        end
      fun c (function,write,fprint,ctable,constant,prof) =
	let val _ = debugmsg "closure"
	    val (function,known,unknown) =
			Closure.closeCPS(function,constant,prof)
	    val constant = fn w => constant w orelse known w orelse unknown w
        in  write "\nAfter closure:\n"; fprint function;
	    if !CPSoption.printsize
		then case function of (_,_,ce) => CPSsize.printsize ce else ();
	    (function,write,fprint,ctable,constant,known,prof)
        end
      fun d (function,write,fprint,ctable,constant,known,prof) =
	let val _ = debugmsg "globalfix"
	    val carg = GlobalFix.globalfix(function,known)
	in  write "\nAfter globalfix:\n"; app fprint (map #1 carg);
	    (carg,write,fprint,ctable,constant,prof)
        end
      fun e (carg,write,fprint,ctable,constant,prof) =
	let val _ = debugmsg "spill"
	    val carg = Spill.spill(carg,constant,prof)
	in  write "\nAfter spill:\n"; app fprint (map #1 carg);
	    write "\n";
	    (carg,ctable)
	end
      fun f (carg,ctable) =
	let val _ = debugmsg "cpscodegen"
        in  CPSg.codegen(carg,ctable);
	    stop();
	    debugmsg "done"; ()
	end
      fun time (f,m,s) x =
        let val t = System.Timer.start_timer()
            val r = f x
	    val t' = System.Timer.check_timer t
        in  System.Stats.update(s,t');
	    timemsg(m ^ ": " ^ System.Timer.makestring t' ^ "s");
	    r
        end
      val a = time(a,"convert",System.Stats.convert)
      val b = time(b,"cpsopt",System.Stats.cpsopt)
      val c = time(c,"closure",System.Stats.closure)
      val d = time(d,"globalfix",System.Stats.globalfix)
      val e = time(e,"spill",System.Stats.spill)
      val f = time(f,"codegen",System.Stats.codegen)
      fun p arg = f(e(d(c(b(a arg)))))
  in  Env.commit();
      process(fname, SOME p)
  end

fun bootEnv code =
 (PreBatch.reprime();
  load "boot/assembly.sig";
  (if code then compile else load) "boot/core.sml";
  load "boot/dummy.sml";
  EnvAccess.setPervasives [EnvAccess.lookSTR (Symbols.stringToSymbol "Core")];
  load "boot/perv.sig";
  load "boot/system.sig";
  (if code then compile else load) "boot/math.sml";
  (if code then compile else load) "boot/perv.sml";
  load "boot/overloads.sml";
  load "boot/opener.sml";
  map ((fn STRvar{access=PATH[v],...} => v)
	o EnvAccess.lookSTR o Symbols.stringToSymbol)
      ["Core","Initial","NewMath"])

end (* functor Batch *)
