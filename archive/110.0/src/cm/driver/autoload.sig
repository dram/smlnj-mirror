(*
 * driver/autoload.sig: CM autoloader
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature AUTO_LOAD = sig

    structure Compiler: COMPILER
    structure SD: SYS_DAG

    type envRef = Compiler.EnvRef.envref
    type SCenvRef = Compiler.EnvRef.SCenvref

    val register: SD.desc * SD.analyzed_entity -> unit
    val manager: Compiler.Ast.dec * SCenvRef * envRef -> unit
    val clear: unit -> unit
    val autolist: unit -> SD.desc list

end
