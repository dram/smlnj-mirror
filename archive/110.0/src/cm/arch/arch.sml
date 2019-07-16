(*
 * arch/arch.sml: CPU architectures and OS configuration for CM
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure Arch:> ARCH = struct

    exception  BadConf of string and BadCpu of string and BadOS of string

    structure S = SMLofNJ.SysInfo

    type cpu = string
    type os = SMLofNJ.SysInfo.os_kind
    type conf = { cpu: cpu, os: os }

    fun os "unix" = S.UNIX
      | os "win32" = S.WIN32
      | os "macos" = S.MACOS
      | os "os2" = S.OS2
      | os "beos" = S.BEOS
      | os s = raise BadOS s

    fun cpu s =
	case s of
	    "alpha32" => s
	  | "alpha32x" => s
	  | "sparc" => s
	  | "mipsel" => s
	  | "mipseb" => s
	  | "hppa" => s
	  | "rs6000" => s
	  | "x86" => s
	  | _ => raise BadCpu s

    fun cpuname cpu = cpu

    fun cpusym "alpha32"   = "Alpha32"
      | cpusym "alpha32x"  = "Alpha32X"
      | cpusym "sparc"     = "Sparc"
      | cpusym "mipsel"    = "MipsLittle"
      | cpusym "mipseb"    = "MipsBig"
      | cpusym "hppa"      = "Hppa"
      | cpusym "rs6000"    = "RS6000"
      | cpusym "x86"       = "X86"
      | cpusym cpu = raise BadCpu cpu

    fun osname S.UNIX = "unix"
      | osname S.WIN32 = "win32"
      | osname S.MACOS = "macos"
      | osname S.OS2 = "os2"
      | osname S.BEOS = "beos"

    fun confname { cpu, os } = concat [cpu, "-", osname os]

end
