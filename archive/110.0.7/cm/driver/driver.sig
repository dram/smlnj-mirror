(*
 * driver/driver.sig:
 *   Invoking the dependency analysis and drive some other actions
 *   using the resulting dependency graph
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature DRIVER = sig

    structure Compiler: COMPILER
    structure SysDag: SYS_DAG

    type senv sharing type senv = Compiler.Environment.staticEnv

    val driver: (SysDag.analyzed_entity -> 'a) * senv -> SysDag.desc -> 'a
    val sysenv'driver: (SysDag.analyzed_entity -> 'a) -> SysDag.desc -> 'a

end
