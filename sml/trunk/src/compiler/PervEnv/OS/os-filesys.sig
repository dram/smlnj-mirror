(* os-filesys.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The generic file system interface.
 *
 *)

signature OS_FILE_SYS =
  sig

    type dirstream

    val openDir   : string -> dirstream
    val readDir   : dirstream -> string
    val rewindDir : dirstream -> unit
    val closeDir  : dirstream -> unit

    val chDir  : string -> unit
    val getDir : unit -> string
    val mkDir  : string -> unit
    val rmDir  : string -> unit
    val isDir  : string -> bool

    val isLink   : string -> bool
    val readLink : string -> string

    val fullPath : string -> string
    val realPath : string -> string

    val fileSize : string -> Position.int
    val modTime  : string -> Time.time
    val setTime  : (string * Time.time option) -> unit
    val remove   : string -> unit
    val rename   : {old : string, new : string} -> unit

    datatype access_mode = A_READ | A_WRITE | A_EXEC

    val access : (string * access_mode list) -> bool

    val tmpName : unit -> string

    eqtype file_id
    val fileId  : string -> file_id
    val hash    : file_id -> word
    val compare : (file_id * file_id) -> order

  end; (* FILE_SYS *)


(*
 * $Log: os-filesys.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)
