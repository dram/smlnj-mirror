(* Copyright 1989 by AT&T Bell Laboratories *)
signature ERRORMSG =
 sig
    exception Syntax
    exception Cascade of string
    datatype severity = WARN | COMPLAIN | CONDEMN | CASCADE | BUG
    type pos (* = int *)
    type pos2 (* = pos * pos *)
    type complainer (*  = severity -> string -> unit *)
    type inputSource (* = {fileName: string,  linePos: int list ref,
	  		    lineNum: int ref, anyErrors: bool ref,
			    errStream: outstream, interactive: bool,
			    sourceStream: instream} *)
    val newSource: string * instream * bool * outstream -> inputSource
    val filepos: inputSource -> int -> string * int * int
    val error:  inputSource -> pos2 -> complainer
    val say : string -> unit
    val warn : string -> unit
    val complain : string -> unit
    val impossible : string -> 'a
 end
