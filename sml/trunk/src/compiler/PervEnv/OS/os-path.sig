(* os-path.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The generic interface to syntactic pathname manipulation.
 *
 *)

signature OS_PATH =
  sig

    exception Path

    val parentArc  : string
    val currentArc : string

    val validVolume : {isAbs : bool, vol : string} -> bool

    val fromString : string -> {isAbs : bool, vol : string, arcs : string list}
    val toString   : {isAbs : bool, vol : string, arcs : string list} -> string

    val getVolume   : string -> string
    val getParent   : string -> string

    val splitDirFile : string -> {dir : string, file : string}
    val joinDirFile  : {dir : string, file : string} -> string
    val dir	     : string -> string
    val file	     : string -> string
    
    val splitBaseExt : string -> {base : string, ext : string option}
    val joinBaseExt  : {base : string, ext : string option} -> string
    val base	     : string -> string    
    val ext	     : string -> string option

    val mkCanonical : string -> string
    val isCanonical : string -> bool

    val mkAbsolute  : {path : string, relativeTo : string} -> string
    val mkRelative  : {path : string, relativeTo : string} -> string
    val isAbsolute  : string -> bool
    val isRelative  : string -> bool

    val isRoot      : string -> bool

    val concat      : (string * string) -> string

  end; (* OS_PATH *)

(*
 * $Log: os-path.sig,v $
 * Revision 1.2  1998/08/21 17:29:23  jhr
 *   Changed types of mkRelative and mkAbsolute to track basis changes.
 *
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)
