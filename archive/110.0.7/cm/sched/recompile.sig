(*
 * sched/recompile.sig: selective recompilation
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature RECOMPILE = sig

    structure SysDag: SYS_DAG
    structure Compiler: COMPILER
    sharing SysDag.ModuleName.Compiler = Compiler

    exception RecompileInternalError
    and WrongConfiguration of string * string
    and CompilationErrors of exn list

    val recomp_only:
	SysDag.analyzed_entity *
	(Compiler.Environment.staticEnv * Compiler.Environment.symenv)
	-> Compiler.Environment.staticEnv * Compiler.Environment.symenv

    val only: SysDag.desc -> unit
    val and'stabilize: bool -> SysDag.desc -> unit
    val and'run: Arch.conf * Arch.conf * string list option -> SysDag.desc -> unit

    val and'run'once:
	SysDag.analyzed_entity * Compiler.Environment.environment
	->
	Compiler.Environment.environment

    val withAe:
	(SysDag.desc * SysDag.analyzed_entity -> unit)
	-> SysDag.desc -> unit

end
