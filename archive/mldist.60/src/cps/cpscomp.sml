(* Copyright 1989 by AT&T Bell Laboratories *)
functor CPScomp(CM : CMACHINE) : 
		sig val compile : CPS.function * System.Unsafe.object option * ErrorMsg.complainer -> unit 
		end =
struct

structure CPSg = CPSgen(CM)
structure CPSopt = CPSopt(val maxfree = 3+length(CM.miscregs))
structure Closure = Closure(val maxfree = 3+length(CM.miscregs))
structure Spill = Spill(val maxfree = 3+length(CM.miscregs))

open ErrorMsg Access Basics BareAbsyn ProcessFile

 fun write s = (if !System.Control.CG.printit then outputc std_out s else ())

    fun debugmsg  (msg : string) =
	let val printit = !System.Control.debugging
	in  if printit then (print msg; print "\n")
	    else ();
	    printit
	end

 fun time (f,m,s) x =
        let val _ = debugmsg m
	    val t = System.Timer.start_timer()
            val r = f x
	    val t' = System.Timer.check_timer t
            val _ = (write "After "; write m; write ":\n")
        in  System.Stats.update(s,t');
	    timemsg(m ^ ": " ^ System.Timer.makestring t' ^ "s");
	    flush_out(std_out);
	    r
        end

fun compile(function,argument,err) =
 let
  fun fprint (function as (f,vl,cps)) =
	  (if !System.Control.CG.printit
		then CPSprint.show write
				(CPS.FIX([function],CPS.PRIMOP(P.+,[],[],[])))
		else ();
	   if !System.Control.CG.printsize then CPSsize.printsize cps else ())
  val _ = fprint function;

  val cpsopt = if !System.Control.CG.cpsopt
		then time(CPSopt.reduce,"cpsopt",System.Stats.cpsopt)
		else fn (cps,_) => cps
  val function = cpsopt(function,argument)
  fun prof(a,b,ce) = CPS.PRIMOP(P.profile, [CPS.INT a, CPS.INT b],nil,[ce])
  val _ = fprint function

  val closure   = time(Closure.closeCPS,"closure",System.Stats.closure)
  val (function,known) = closure(function,prof)
  val _ = fprint function

  val globalfix = time(GlobalFix.globalfix,"globalfix",System.Stats.globalfix)
  val carg = globalfix(function,known)
  val _ = app fprint (map #1 carg)

  val spill     = time(Spill.spill,"spill",System.Stats.spill)
  val carg = spill(carg,prof)
  val _ = (app fprint (map #1 carg); write "\n")

  val codegen   = time(CPSg.codegen,"codegen",System.Stats.codegen)
  val _ = codegen(carg,err)
  val _ = debugmsg "done"
  in ()
 end
end (* functor CPScomp *)
