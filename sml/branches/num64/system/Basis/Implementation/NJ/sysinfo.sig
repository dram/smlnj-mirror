(* sysinfo.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Get information about the underlying hardware and OS.
 *)

signature SYS_INFO =
  sig

    exception UNKNOWN
	(* this exception is raised when the runtime cannot provide the
	 * requested information.
	 *)

    datatype os_kind
      = UNIX	(* one of the many flavours of UNIX (incl macOS and Linux) *)
      | WIN32	(* Win32 API *)

    val getOSKind    : unit -> os_kind
    val getOSName    : unit -> string
    val getOSVersion : unit -> string

    val getHostSize : unit -> int
	(* returns word size of the host architecture (either 32 or 64) *)

    val getHostArch   : unit -> string
	(* returns the HOST_ARCH value from the run-time build *)
    val getTargetArch : unit -> string
	(* returns the TARGET_ARCH value from the run-time build; this is
	 * usually the same as the host architecture, except in the case that
	 * some form of emulation is being run (e.g., ML-to-C, or an
	 * interpreter).
	 *)

    val hasSoftwarePolling : unit -> bool
	(* returns true, if the run-time system was compiled to support software
	 * polling.
	 *)

    val hasMultiprocessing : unit -> bool
	(* returns true, if the run-time system was compiled to support the
	 * multiprocessing hooks.  This does not mean that the underlying
	 * hardware is a multiprocessor.
	 *)

    val getHeapSuffix : unit -> string

  end
