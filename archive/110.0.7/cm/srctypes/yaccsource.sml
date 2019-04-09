(*
 * srctypes/yaccsource.sml: dealing with input for ml-yacc
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor YaccSourceFun (structure Tools: TOOLS
		       val command: string) =
struct

    local
	val runyacc =
	    Tools.stdShellProcessor { command = command, tool = "ML-Yacc" }

	fun rule source = let
	    val smlfile = source ^ ".sml"
	    val sigfile = source ^ ".sig"
	    fun sml f = (f, SOME "sml")
	in
	    [sml sigfile, sml smlfile]
	end

	val validator = Tools.stdTStampValidator

	val processor = runyacc

	(* install MlYacc class *)
	open Tools
	val class = "mlyacc"
	fun sfx s = addClassifier (stdSfxClassifier { sfx = s, class = class })
    in
	val _ = addToolClass { class = class,
			       rule = dontcare rule,
			       validator = validator,
			       processor = processor }
	val _ = sfx "grm"
	val _ = sfx "y"
    end
end
