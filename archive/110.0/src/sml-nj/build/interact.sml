(* interact.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature INTERACT =
sig
  exception Interrupt

  val interact : unit -> unit
  val useFile : string -> unit
  val useStream : TextIO.instream -> unit
  val evalStream : TextIO.instream * SCEnv.Env.environment -> 
                       SCEnv.Env.environment

  val installCompManager:
      (Ast.dec *
       { get: unit -> SCEnv.Env.environment,
	 set: SCEnv.Env.environment -> unit } *
       { get: unit -> Environment.environment,
	 set: Environment.environment -> unit }
       -> unit) option
      -> unit
end

functor Interact(EvalLoop : EVALLOOP) : INTERACT =
struct
  exception Interrupt = EvalLoop.Interrupt

 (*
  * This is where CM can install itelf into.  Added for the purpose of
  * autoloading. (blume)
  *)
  type envref = EnvRef.envref

  fun installCompManager m = (#compManagerHook EvalLoop.stdParams) := m

  fun interact() = (
	EvalLoop.interact EvalLoop.stdParams;
	OS.Process.exit OS.Process.success)

  fun useFile (fname: string) =
      (app Control.Print.say ["[opening ",fname,"]\n"];
       EvalLoop.evalStream EvalLoop.stdParams
		  (fname,(TextIO.openIn fname
			  handle e as IO.Io _ =>
			      (app Control.Print.say["[use failed: ",
						     General.exnMessage e,
						     "]\n"];
			       raise ErrorMsg.Error))))

  fun useStream (stream: TextIO.instream) =
      EvalLoop.evalStream EvalLoop.stdParams ("<instream>", stream)

  fun evalStream (stream: TextIO.instream, baseEnv: SCEnv.Env.environment) : 
      SCEnv.Env.environment =
      let val r = ref Environment.emptyEnv
	  val localEnvRef = {get=(fn()=> !r),set=(fn x=>r:=x)}
	  val b = ref baseEnv
	  val baseEnvRef = {get=(fn()=> !b),set=(fn _ => raise Fail "evalStream")}
       in EvalLoop.evalStream
	    ({compManagerHook = ref NONE,
	      (* ????  should CM get its hands into that? *)
	      baseEnvRef = baseEnvRef,
	      localEnvRef=localEnvRef,
	      transform=(fn x => x), instrument=(fn _ => fn x => x),
	      perform=(fn x => x),
	      isolate= #isolate EvalLoop.stdParams,
	      printer= #printer EvalLoop.stdParams})
	    ("<instream>", stream);
	  SCEnv.SC (#get localEnvRef ())
      end

end (* functor Interact *)





(*
 * $Log: interact.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/09/22 17:36:39  appel
 * Eliminate build/topcompile.sml by merging it with build/compile.sml
 *
 * Revision 1.2  1997/08/02  02:12:18  dbm
 *   New top level using Environment.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:28  george
 *   Version 109.24
 *
 *)
