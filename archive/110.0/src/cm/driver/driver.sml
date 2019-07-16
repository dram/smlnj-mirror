(*
 * driver/driver.sml:
 *   Invoking the dependency analysis and drive some other actions
 *   using the resulting dependency graph
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
local
  val analysisPhase = Stats.newPhase "Dependency analysis"
  fun inAnalysisPhase f = Stats.inPhase analysisPhase f
in
  functor DriverFun (structure SysDag: SYS_DAG
		     structure Control: CONTROL
		     structure SysEnv: SYS_ENV
		     val path: AbsPath.t list ref
		     val symval: string -> int option
		     sharing
			 SysDag.GroupDag.SmlSource.Compiler =
			 SysEnv.Compiler): DRIVER = 

  struct

    structure SysDag = SysDag
    structure EntityDesc = SysDag.EntityDesc
    structure SmlSource = SysDag.GroupDag.SmlSource
    structure Compiler = SmlSource.Compiler

    type senv = Compiler.Environment.staticEnv

    fun driver (action, staticBaseEnv) groupfile = let
	fun symdef s = let
	    val bsbe = Compiler.EnvRef.unSCstaticEnv staticBaseEnv
	in
	    case Compiler.BareEnvironment.cmEnvOfModule bsbe s of
		Compiler.BareEnvironment.CM_NONE => false
	      | _ => true
	end
	val strdef = symdef o Compiler.Symbol.strSymbol
	val sigdef = symdef o Compiler.Symbol.sigSymbol
	val fctdef = symdef o Compiler.Symbol.fctSymbol
	val fsigdef = symdef o Compiler.Symbol.fsigSymbol
	val ep = EntityDesc.EP {
				path = !path,
				lparm = {
					 strdef = strdef,
					 sigdef = sigdef,
					 fctdef = fctdef,
					 fsigdef = fsigdef,
					 symval = symval
					}
			       }
	val _ =
	    if AbsPath.newcwd () then
		(SmlSource.clearcache (); EntityDesc.clear ())
	    else ()
	val _ = Control.vsay "[starting dependency analysis]\n"
	val ae = inAnalysisPhase SysDag.analyze (ep, groupfile, staticBaseEnv)
	val _ = Control.vsay "[dependency analysis completed]\n"
    in
	action ae
    end

    fun sysenv'driver action groupfile =
	driver (action, SysEnv.getStaticBaseEnv ()) groupfile

  end
end
