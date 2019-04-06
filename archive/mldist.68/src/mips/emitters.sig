(* We have an odd problem---we need to be able to emit either a 32-bit      *)
(* integer or a string.                                                     *)
(* The order in which the bytes of the integer are emitted depends on       *)
(* whether the target machine is BigEndian or LittleEndian, but the         *)
(* bytes of the string should be emitted in the same order on both machines. *)
(* This means that the two emission functions depend on each other, but     *)
(* in a machine-dependent way, so we bundle them up.                        *)
(* We also have to be able to emit two words for floating point constants.  *)
(* The way to do this can be derived from [[emit_word]], and this           *)
(* code seems to be the sensible place to do that.                          *)
(* So we define a type [[emitter_triple]]                                   *)
(* and pass them around that way.                                           *)
(*                                                                          *)

(* Eventually we want to take all the words and strings that have been      *)
(* emitted and bundle them up into a single string using [[implode]].       *)
(* We'll take the following tack with that:                                 *)
(* each emitter will squirrel some info away in a reference variable.       *)
(* A function [[emitted_string: unit -> string]] will take the              *)
(* squirreled information and return a string that represents               *)
(* everything emitted so far.                                               *)
(* As a side effect, it will reset the emitter system to its initial        *)
(* state, where ``everything emitted so far'' is the empty string.          *)
(*                                                                          *)
(* The actual implementation will be a list of strings which is reversed    *)
(* and imploded.                                                            *)

signature EMITTER = sig val emit: int * int -> unit
			val order_real: string -> string
			val low_order_offset: int
			val emit_string: int -> string -> unit
			val emit_comment: string -> unit
		    end

signature EMITTERS = sig
    structure LittleEndian : EMITTER
    structure BigEndian : EMITTER
    structure MipsAsm: EMITTER
    val emitted_string : unit -> string
    val address : int ref
    val asmstream: outstream ref
end

