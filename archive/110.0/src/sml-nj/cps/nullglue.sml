(* nullglue.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 *)
structure IntNull = IntShare(
structure Machm = struct
                val _ = Control.interp := true;
                fun generate lexp = ErrorMsg.impossible "no code generator!"
             end
  val fileExtension = ".nul"
  functor Debugger = RealDebugger);
structure IntNullD = IntShare(
structure Machm = struct
                val _ = Control.interp := true;
                fun generate lexp = ErrorMsg.impossible "no code generator!"
             end
  val fileExtension = ".nul"
  functor Debugger = RealDebugger);

(*
 * $Log: nullglue.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:44  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:32  george
 *   Version 109.24
 *
 *)
