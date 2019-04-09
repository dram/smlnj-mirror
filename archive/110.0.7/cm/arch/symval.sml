(*
 * arch/symval.sml: CM preprocessor symbols
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor SymValFun (val conf: Arch.conf
		   val version: int list): SYMVAL =
struct

    val mapping: (string, int) Table.table = Table.create (op =)

    fun lookup s = Table.find (mapping, s)

    fun define (s, i) = Table.enter (mapping, s, i)

    fun undef s = let
	val l = Table.fold (fn (s, i, a) => (s, i) :: a) mapping []
	val _ = Table.clear mapping
	fun def (s1, i1) = if s = s1 then () else define (s1, i1)
    in
	app def l
    end

    fun undefall () = Table.clear mapping

    fun default { conf = { cpu, os }, version } = let

	val cpusyms =
	    case Arch.cpuname cpu of
		"sparc" => ["ARCH_SPARC", "BIG_ENDIAN", "SIZE_32"]
	      | "alpha32" => ["ARCH_ALPHA", "LITTLE_ENDIAN", "SIZE_32"]
	      | "alpha32x" => ["ARCH_ALPHA", "LITTLE_ENDIAN", "SIZE_32"]
	      | "mipsel" => ["ARCH_MIPS", "LITTLE_ENDIAN", "SIZE_32"]
	      | "mipseb" => ["ARCH_MIPS", "BIG_ENDIAN", "SIZE_32"]
	      | "x86" => ["ARCH_X86", "LITTLE_ENDIAN", "SIZE_32"]
	      | "hppa" => ["ARCH_HPPA", "LITTLE_ENDIAN", "SIZE_32"]
	      | "rs6000" => ["ARCH_RS6000", "LITTLE_ENDIAN", "SIZE_32"]
	      | _ => []

	val ossyms =
	    case os of
		SMLofNJ.SysInfo.UNIX => ["OPSYS_UNIX"]
	      | SMLofNJ.SysInfo.WIN32 => ["OPSYS_WIN32"]
	      | SMLofNJ.SysInfo.MACOS => ["OPSYS_MACOS"]
	      | SMLofNJ.SysInfo.OS2 => ["OPSYS_OS2"]
	      | SMLofNJ.SysInfo.BEOS => ["OPSYS_BEOS"]

	val (major, minor) =
	    case version of
		[] => (0, 0)
	      | [major] => (major, 0)
	      | (major :: minor :: _) => (major, minor)
		    
    in
	undefall ();
	app (fn s => define (s, 1)) cpusyms;
	app (fn s => define (s, 1)) ossyms;
	define ("SMLNJ_VERSION", major);
	define ("SMLNJ_MINOR_VERSION", minor)
    end

    val _ = default { conf = conf, version = version }

end
