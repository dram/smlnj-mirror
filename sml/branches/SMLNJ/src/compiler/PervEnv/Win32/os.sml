(* os.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Win32 OS interface
 *
 *)

structure OS : OS =
  struct

    open OS (* open type-only structure to get types *)

    type syserror = int

(*    exception SysErr of (string * syserror option)  *)
    exception SysErr = Assembly.SysErr

    fun errorName _ = "<OS.errorName unimplemented>"
    fun errorMsg _ = "<OS.errorMessage unimplemented>"
    fun syserror _ = raise Fail "OS.syserror unimplemented"

    structure FileSys = OS_FileSys
    structure Path = OS_Path
    structure Process = OS_Process
    structure IO = OS_IO

  end (* OS *)

(*
 * $Log: os.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:40:02  george
 * Version 110.5
 *
 *)
