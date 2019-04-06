(* basicsparc.sig
 *
 * J.H. Reppy (3/15/89)
 * Cornell University
 * Ithaca, NY  14853
 * jhr@cs.cornell.edu
 *)

signature BASICSPARC =
sig

    eqtype Label

    datatype register = REG of int

    datatype fregister = FREG of int

    datatype reg_or_immed
	=   REGrand of register
	|   IMrand of int

  (* misc operations *)
    val align : unit -> unit
    val mark : unit -> unit

  (* emit constants into the code stream *)
    val emitLong : int -> unit
    val emitReal : string -> unit
    val emitString : string -> unit

  (* label operations *)
    val emitLab : (int * Label) -> unit
    val newlabel : unit -> Label
    val define : Label -> unit

  (* Span dependent psuedo-instructions for handling relative addresses. *)
    val loadAdr : (Label * int * register * register) -> unit
    val adjustAdr : (Label * register) -> unit

  (* span dependent conditional jumps (psuedo instructions) *)
    val emit_ba : Label -> unit
    val emit_be : Label -> unit
    val emit_bne : Label -> unit
    val emit_ble : Label -> unit
    val emit_bge : Label -> unit
    val emit_bl : Label -> unit
    val emit_bg : Label -> unit
    val emit_fbe : Label -> unit
    val emit_fbne : Label -> unit
    val emit_fble : Label -> unit
    val emit_fbge : Label -> unit
    val emit_fbl : Label -> unit
    val emit_fbg : Label -> unit

  (* nop *)
    val emit_nop : unit -> unit

  (* sethi - set the high 22 bits of a register *)
    val emit_sethi : (int * register) -> unit

  (* ld - load a register from memory (3rd arg is dst) *)
    val emit_ld : (register * reg_or_immed * register) -> unit
  (* st - store a register into memory (3rd arg is src) *)
    val emit_st : (register * reg_or_immed * register) -> unit

  (* ldf - load a floating-point register from memory (3rd arg is dst) *)
    val emit_ldf : (register * reg_or_immed * fregister) -> unit
  (* stf - store a floating-point register into memory (3rd arg is src) *)
    val emit_stf : (register * reg_or_immed * fregister) -> unit

  (* load/store (unsigned) byte instructions (3rd arg is dst/src) *)
    val emit_ldb : (register * reg_or_immed * register) -> unit
    val emit_stb : (register * reg_or_immed * register) -> unit

  (* jmpl - jump and link *)
    val emit_jmpl : (register * reg_or_immed * register) -> unit

  (* integer operations *)
    val emit_add : (register * reg_or_immed * register) -> unit
    val emit_addcc : (register * reg_or_immed * register) -> unit
    val emit_sub : (register * reg_or_immed * register) -> unit
    val emit_subcc : (register * reg_or_immed * register) -> unit
    val emit_sll : (register * reg_or_immed * register) -> unit
    val emit_sra : (register * reg_or_immed * register) -> unit
    val emit_and : (register * reg_or_immed * register) -> unit
    val emit_andcc : (register * reg_or_immed * register) -> unit
    val emit_or : (register * reg_or_immed * register) -> unit
    val emit_xor : (register * reg_or_immed * register) -> unit
    val emit_not : (register * register) -> unit    (* really xnor *)

  (* floating-point operations *)
    val emit_fadd : (fregister * fregister * fregister) -> unit
    val emit_fsub : (fregister * fregister * fregister) -> unit
    val emit_fmul : (fregister * fregister * fregister) -> unit
    val emit_fdiv : (fregister * fregister * fregister) -> unit
    val emit_fcmp : (fregister * fregister) -> unit
    val emit_fneg : (fregister * fregister) -> unit

  (* trap on integer overflow *)
    val emit_tvs : unit -> unit

    val comment : string -> unit

end (* signature BASICSPARC *)

signature SPARCMCODER =
sig
  structure Coder : BASICSPARC
  val finish : unit -> string
end
