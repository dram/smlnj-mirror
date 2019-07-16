(*
 * sys/sysenv.sig: CM's access to base environment
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature SYS_ENV = sig

    structure Compiler: COMPILER

    val getBaseEnv: unit -> Compiler.Environment.environment
    val getStaticBaseEnv: unit -> Compiler.Environment.staticEnv
    val addToInteractiveEnv: Compiler.Environment.environment -> unit

end
