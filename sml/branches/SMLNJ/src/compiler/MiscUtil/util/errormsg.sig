(* <errormsg.sig>=                                                          *)
(* Copyright 1989 by AT&T Bell Laboratories *)
signature ERRORMSG =
 sig
    datatype severity = WARN | COMPLAIN
    type complainer (*  = severity -> string -> (PrettyPrint.ppstream -> unit)
                          -> unit *)
    type errorFn = SourceMap.region -> complainer
    type errors (* = {error: errorFn,
                      errorMatch: region->string,
                      anyErrors : bool ref} *)
    val anyErrors : errors -> bool
    exception Error
    val defaultConsumer : unit -> PrettyPrint.ppconsumer
    val nullErrorBody : PrettyPrint.ppstream -> unit
    val error : Source.inputSource -> SourceMap.region -> complainer
    val errorNoFile : PrettyPrint.ppconsumer * bool ref -> SourceMap.region
                      -> complainer

    val matchErrorString : Source.inputSource -> SourceMap.region -> string
    val errors : Source.inputSource -> errors
    val errorsNoFile : PrettyPrint.ppconsumer * bool ref -> errors

    val impossible : string -> 'a
    val impossibleWithBody : string -> (PrettyPrint.ppstream -> unit) -> 'a
 end

(*
 * $Log: errormsg.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:16  george
 * Version 110.5
 *
 *)
