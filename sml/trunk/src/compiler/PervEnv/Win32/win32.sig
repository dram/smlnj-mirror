(* win32.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Signature for the interface to Win32.
 *
 *)

signature WIN32 =
    sig
	structure General : WIN32_GENERAL
	structure FileSys : WIN32_FILESYS
	structure IO      : WIN32_IO
	structure Process : WIN32_PROCESS
    end 


(*
 * $Log$
 *)
