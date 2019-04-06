functor CPScomp(CM : CMACHINE) : 
		sig val compile : Lambda.lexp -> unit end =
struct

structure CPSg = CPSgen(CM)
structure CPSopt = CPSopt(val maxfree = 3+length(CM.miscregs))
structure Closure = Closure(val maxfree = 3+length(CM.miscregs))
structure Spill = Spill(val maxfree = 3+length(CM.miscregs))

open ErrorMsg Access Basics BareAbsyn ProcessFile

  local fun a (lexp) =
	let val write = CM.comment
	    val _ = debugmsg "convert"
	    val (function,ctable) = Convert.convert lexp
	    fun fprint (function : CPS.function) =
	      (if !System.Control.CG.printit
		then CPSprint.show write (Intmap.map ctable)
				(CPS.FIX([function],CPS.PRIMOP(P.+,[],[],[])))
		else ())
	in  write "After convert:\n"; fprint function;
	    if !System.Control.CG.printsize
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
	    fun prof(a,b,ce) = CPS.PRIMOP(P.profile,
				    [newconst a,newconst b],nil,[ce])
	    val ctable = Intmap.map ctable
	    val constant = fn w => ((ctable w; true)
						handle Ctable => false)
        in  write "\nAfter cpsopt:\n"; fprint function;
	    if !System.Control.CG.printsize
		then case function of (_,_,ce) => CPSsize.printsize ce else ();
	    (function,write,fprint,ctable,constant,prof)
        end
      fun c (function,write,fprint,ctable,constant,prof) =
	let val _ = debugmsg "closure"
	    val (function,known,unknown) =
			Closure.closeCPS(function,constant,prof)
	    val constant = fn w => constant w orelse known w orelse unknown w
        in  write "\nAfter closure:\n"; fprint function;
	    if !System.Control.CG.printsize
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
	    val constant' = 
		let val s = Intset.new()
		    val _ = app (Intset.add s o #1 o #1) carg
		    val isfun = Intset.mem s
		 in fn v => constant v orelse isfun v
	        end
	    val carg = Spill.spill(carg,constant',prof)
	in  write "\nAfter spill:\n"; app fprint (map #1 carg);
	    write "\n";
	    (carg,ctable,constant')
	end
      fun f (carg,ctable,constant') =
	let val _ = debugmsg "cpscodegen"
        in  CPSg.codegen(carg,ctable,constant');
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
      
  in  fun compile arg = f(e(d(c(b(a arg)))))
 end

end (* functor CPScomp *)
