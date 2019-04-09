(*
 * srctypes/fname-rules.sml:
 *   Rules for how to make up names for CM-managed files
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor FnameRulesFun (structure Compiler: COMPILER
		       structure Control: CONTROL
		       val singlebindir: AbsPath.t option
		       val namelength_limited: bool
		       val targetos: Arch.os): FNAME_RULES =
  struct

    val arch = Compiler.architecture
    val sep = "."

    fun add_ext NONE f = f
      | add_ext (SOME e) f = let
	val { base, ext } = AbsPath.splitBaseExt f
	val new_ext =
	    case ext of
		NONE => SOME e
	      | SOME old_ext => SOME (concat [old_ext, sep, e])
    in
	AbsPath.joinBaseExt { base = base, ext = new_ext }
    end

    val bin_ext = if namelength_limited then NONE else SOME "bin"
    val stable_ext = if namelength_limited then NONE else SOME "stable"

    val cmdirname = "CM"

    val confdirname = concat [arch, "-", Arch.osname targetos]
    val decldirname = "DEPEND"

    fun subdir (dir, "") = dir
      | subdir (dir, arc) = AbsPath.joinDirFile { dir = dir, file = arc }

    fun genericFileFor (gendir, suf) path = let
	val { dir, file } = AbsPath.splitDirFile path
	val cmdir = subdir (dir, cmdirname)
	val dir = subdir (cmdir, gendir)
	val base = AbsPath.joinDirFile { dir = dir, file = file }
    in
	add_ext suf base
    end

    val declFileFor = genericFileFor (decldirname, NONE)
    val stableFileFor = genericFileFor (confdirname, stable_ext)

    val binFileFor =
	case singlebindir of
	    NONE => genericFileFor (confdirname, bin_ext)
	  | SOME d => let
		(*
		 * `Batch' compilation drops all the binfiles into
		 * one single directory -- let's pray there are no duplicate
		 * source names in different source directories...
		 *)
		fun binFileFor name = let
		    val base = AbsPath.joinDirFile { dir = d,
						     file = AbsPath.file name }
		in
		    add_ext bin_ext base
		end
	    in
		binFileFor
	    end


    val errorTextFile = AbsPath.dummy

  end
