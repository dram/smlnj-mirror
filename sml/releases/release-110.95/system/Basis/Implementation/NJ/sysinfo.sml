(* sysinfo.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Get information about the underlying hardware and OS.
 *)

structure SysInfo : SYS_INFO =
  struct

    exception UNKNOWN

    fun getInfoStr NONE = raise UNKNOWN
      | getInfoStr (SOME s) = s

    datatype os_kind
      = UNIX	(* one of the many flavours of UNIX (incl macOS and Linux) *)
      | WIN32	(* Win32 API *)

    fun sysInfo (s: string): string option =
	  CInterface.c_function "SMLNJ-RunT" "sysInfo" s
    fun getFlag flag = (case (getInfoStr(sysInfo flag))
	   of "NO" => false
	    | _ => true
	  (* end case *))

    fun getOSName () = getInfoStr(sysInfo "OS_NAME")
    fun getOSKind () = (case (getOSName())
	   of ("Solaris"|"AIX"|"Linux"|"BSD"|"Darwin"|"Cygwin") => UNIX
	    | "Win32" => WIN32
	    | _ => raise Fail "unknown OS"
	  (* end case *))
    fun getOSVersion () = getInfoStr(sysInfo "OS_VERSION")

    fun getHostSize () = (case getInfoStr(sysInfo "HOST_ARCH")
	   of "AMD64" => 64
	    | _ => 32
	  (* end case *))

    fun getHostArch () = getInfoStr(sysInfo "HOST_ARCH")
    fun getTargetArch () = getInfoStr(sysInfo "TARGET_ARCH")

    fun hasSoftwarePolling () = getFlag "HAS_SOFT_POLL"
    fun hasMultiprocessing () = getFlag "HAS_MP"

    fun getHeapSuffix () = getInfoStr (sysInfo "HEAP_SUFFIX")

  end


