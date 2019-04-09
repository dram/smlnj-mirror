(*
 * sched/gen-lists.sml:
 *   Make various topologically sorted name lists
 *   (given the root description file)
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor GenListsFun (structure Lists: LISTS
		     structure Driver: DRIVER
		     sharing Lists.SysDag = Driver.SysDag): GEN_LISTS = struct

    structure SysDag = Lists.SysDag

    val smls = Driver.sysenv'driver Lists.smls
    val names = Driver.sysenv'driver Lists.names
    val binfiles = Driver.sysenv'driver Lists.binfiles
    val strings = Driver.sysenv'driver Lists.strings

    fun mkusefile (groupfile, listfile) = let
	val names = names groupfile
	val os = TextIO.openOut listfile
	fun p n = TextIO.output (os, "use \"" ^ n ^ "\";\n")
    in
	app p names handle exn => (TextIO.closeOut os; raise exn);
	TextIO.closeOut os
    end

end
