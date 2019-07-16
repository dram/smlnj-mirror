(*
 * dot/gen-dot.sml: interface to DOT generator
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor GenDotFun (structure Dot: DOT
		   structure Driver: DRIVER
		   sharing Dot.SysDag = Driver.SysDag): GEN_DOT =
  struct

    structure SysDag = Dot.SysDag

    fun genDot (groupfile, outfile) = let
	fun action ae = let
	    val outs = TextIO.openOut outfile
	    fun pr s = TextIO.output (outs, s)
	in
	    Dot.dot (pr, ae);
	    TextIO.closeOut outs
	end
    in
	Driver.sysenv'driver action groupfile
    end

  end
