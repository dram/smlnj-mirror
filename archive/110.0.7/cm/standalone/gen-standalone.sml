(*
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor GenSAloneFun (structure SAlone: SALONE
		      structure Driver: DRIVER
		      sharing SAlone.SysDag = Driver.SysDag): GEN_SALONE =
  struct

    structure SysDag = SAlone.SysDag

    fun genStandAlone (groupfile, outfile) = let
	fun action ae = let
	    val outs = TextIO.openOut outfile
	    fun pr s = TextIO.output (outs, s)
	in
	    SAlone.standAlone (pr, ae);
	    TextIO.closeOut outs
	end
    in
	Driver.sysenv'driver action groupfile
    end

  end
