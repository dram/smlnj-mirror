(* os.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature OS =
  sig
    type syserror

    val errorName : syserror -> string
    val syserror : string -> syserror option
    val errorMsg : syserror -> string

    exception SysErr of (string * syserror option)

    structure FileSys : OS_FILE_SYS
    structure Path : OS_PATH
    structure Process : OS_PROCESS
    structure IO : OS_IO

  end;


(*
 * $Log: os.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)
