(*
 * cm/link.sml: Building the CM and CMB toplevel structures
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)

local
    val hostcpu = Arch.cpu Compiler.architecture
    val hostos = SMLofNJ.SysInfo.getOSKind ()
    val hostconf = { cpu = hostcpu, os = hostos }
in

    structure CM: COMPILATION_MANAGER = CompilationManagerFun
	(structure Compiler = Compiler
	 val version = "1"
	 val singlebindir = NONE
	 val hostconf = hostconf
	 val targetos = hostos)

    structure CMB = CMBFun
	(structure Compiler = Compiler
	 val version = "1 (batch)"
	 val hostcpun = Arch.cpuname hostcpu
	 val hostosn = Arch.osname hostos
	 val targetosn = Arch.osname hostos)

    local
	val _ = CM.initCleanup ()
    in
	(* nothing - just shut up CM *)
    end
end
