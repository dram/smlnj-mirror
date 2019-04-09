(*
 * srctypes/rcssource.sml: dealing with RCS archives
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor RcsSourceFun (structure Control: CONTROL
		      structure Tools: TOOLS
		      val command: string) =
struct

    local
	fun runrcsco { source, targets = [(target, _)] } =
	    let
		val cmd = concat [command, " ", target]
		val _ = Control.vsay (concat ["[", cmd, "]\n"])
	    in
		if (OS.Process.system cmd) = OS.Process.success then ()
		else raise Tools.ToolError { tool = "RCS", msg = cmd }
	    end
	  | runrcsco _ = raise Tools.ToolError { tool = "RCS",
						 msg = "internal CM error" }

	fun rule source = let
	    val { dir, file } = OS.Path.splitDirFile source
	    val workfile = substring (file, 0, size file - 2)
	    fun isCur d = d = "" orelse d = OS.Path.currentArc
	    val workdir =
		if isCur dir then dir
		else let
		    val { dir = parentdir, file = subdir } =
			OS.Path.splitDirFile dir
		in
		    if subdir = "RCS" then parentdir else dir
		end
	    val workpath =
		OS.Path.joinDirFile { dir = workdir, file = workfile }
	in
	    [(workpath, NONE)]
	end

	val validator = Tools.stdExistenceValidator

	val processor = runrcsco

	fun classifier name =
	    if size name > 2 andalso
		substring (name, size name - 2, 2) = ",v" then
		SOME "rcs"
	    else
		NONE

	(* install RCS class *)
	open Tools
	val class = "rcs"
    in
	val _ = addToolClass { class = class,
			       rule = dontcare rule,
			       validator = validator,
			       processor = processor }
	val _ = addClassifier (Tools.GEN_CLASSIFIER classifier)
    end
end
