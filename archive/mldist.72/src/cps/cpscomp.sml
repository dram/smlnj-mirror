(* Copyright 1989 by AT&T Bell Laboratories *)
functor CPScomp(CM : CMACHINE) : 
	sig val compile : CPS.function * System.Unsafe.object option * ErrorMsg.complainer -> unit 
	end =
struct

val maxfree = case CM.arithtemps of [] => 3+length(CM.miscregs)-1
                                  | _ => 3+length(CM.miscregs)

structure CPSg = CPSgen(CM)
structure CPSopt = CPSopt(val maxfree = maxfree)
structure Closure = Closure(val maxfree = maxfree)
structure ClosureCallee = ClosureCallee(val maxfree = maxfree)
structure Spill = Spill(val maxfree = maxfree)

 fun timemsg (s : string) =
    if !System.Control.timings then (print s; print "\n"; true) else false

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

 fun fprint (function as (f,vl,cps)) =
	  (if !System.Control.CG.printit
		then CPSprint.show write
			(CPS.FIX([function],CPS.PRIMOP(Access.P.+,[],[],[])))
		else ();
	   if !System.Control.CG.printsize then CPSsize.printsize cps else ())

fun compile(function,argument,err) =
 let
  val _ = fprint function;

  val cpsopt = if !System.Control.CG.cpsopt
		then time(CPSopt.reduce,"cpsopt",System.Stats.cpsopt)
		else fn (cps,_,_) => cps
  val function = cpsopt(function,argument,false)
  fun prof(a,b,ce) = CPS.PRIMOP(Access.P.profile, [CPS.INT a, CPS.INT b],nil,[ce])
  val _ = fprint function

  val closure = if (!System.Control.CG.calleesaves > 0)
                then time(ClosureCallee.closeCPS,"closure",System.Stats.closure)
                else time(Closure.closeCPS,"closure",System.Stats.closure)
  val function = closure(function,prof)
  val _ = fprint function

  val globalfix = time(GlobalFix.globalfix,"globalfix",System.Stats.globalfix)
  val carg = globalfix function
  val _ = app fprint carg


  fun reoptimize((f,vl,body)::carg') = 
           globalfix(cpsopt((f,vl,CPS.FIX(carg',body)),argument,true))

  val carg = if !System.Control.CG.optafterclosure
    then let open System.Control.CG
	     val u = !hoistup and d = !hoistdown and m = !misc2
	 in hoistup := false; hoistdown := false; misc2 := false;
            reoptimize carg            
            before (hoistup := u; hoistdown := d; misc2 := m)
         end
    else carg

  val spill     = time(Spill.spill,"spill",System.Stats.spill)
  val carg = spill(carg,prof)
  val _ = (app fprint carg; write "\n")

(**
  val branch = time(Branch.branch, "branch", System.Stats.spill)
  val carg = if !System.Control.CG.misc3>0 then branch carg else carg
  val _ = (app fprint carg; write "\n")
**)

  val codegen   = time(CPSg.codegen,"generic",System.Stats.codegen)
  val _ = codegen(carg,err)
  val _ = debugmsg "done"
  in ()
 end
end (* functor CPScomp *)
