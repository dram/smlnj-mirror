(*
 * sys/sysenv.sml:
 *   CM's access to base (pervasive) and interactive environment
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor SysEnvFun (Compiler: COMPILER): SYS_ENV = struct

    structure Compiler = Compiler
    structure Env = Compiler.Environment
    structure BareEnv = Compiler.BareEnvironment
    structure EnvRef = Compiler.EnvRef
    structure Sym = Compiler.Symbol

    val getBaseEnv = #get EnvRef.pervasive

    val getStaticBaseEnv = Env.staticPart o getBaseEnv

    fun addToInteractiveEnv delta = let
	val base = # get EnvRef.topLevel ()
	val new = BareEnv.concatEnv (EnvRef.unSCenv delta, base)
    in
        #set EnvRef.topLevel new
    end

end
