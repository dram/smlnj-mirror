(*
 * srctypes/lexsource.sml: dealing with ml-lex input
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor LexSourceFun (structure Tools: TOOLS
		      val command: string) =
struct

    local
	val runlex =
	    Tools.stdShellProcessor { command = command, tool = "ML-Lex" }

	fun rule source = let
	    val smlfile = source ^ ".sml"
	in
	    [(smlfile, SOME "sml")]
	end

	val validator = Tools.stdTStampValidator

	val processor = runlex

	(* install MlLex class *)
	open Tools
	val class = "mllex"
	fun sfx s = addClassifier (stdSfxClassifier { sfx = s, class = class })
    in
	val _ = addToolClass { class = class,
			       rule = dontcare rule,
			       validator = validator,
			       processor = processor }
	val _ = sfx "lex"
	val _ = sfx "l"
    end
end
