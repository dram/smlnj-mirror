(*
 * srctypes/burgsource.sml: dealing with ml-burg input
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor BurgSourceFun (structure Tools: TOOLS
		       val command: string) =
struct

    local
	val runburg =
	    Tools.stdShellProcessor { command = command, tool = "ML-Burg" }

	fun rule source = let
	    val { base, ext } = OS.Path.splitBaseExt source
	    val sml = "sml"
	    val ext = case ext of
		NONE => SOME sml
	      | SOME "burg" => SOME sml
	      | SOME other => SOME (concat [other, ".", sml])
	    val smlfile = OS.Path.joinBaseExt { base = base, ext = ext }
	in
	    [(smlfile, SOME "sml")]
	end

	val validator = Tools.stdTStampValidator

	val processor = runburg

	(* install MlBurg class *)
	open Tools
	val class = "mlburg"
	fun sfx s = addClassifier (stdSfxClassifier { sfx = s, class = class })
    in
	val _ = addToolClass { class = class,
			       rule = dontcare rule,
			       validator = validator,
			       processor = processor }
	val _ = sfx "burg"
    end
end
