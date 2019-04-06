(* sparccoder.sig
 *
 * J.H. Reppy (3/15/89)
 * Cornell University
 * Ithaca, NY  14853
 * jhr@cs.cornell.edu
 *)

signature SPARCCODER =
sig
    eqtype Label

    datatype register = REG of int	(* general-purpose registers *)

    datatype fregister = FREG of int	(* floating-point registers *)

    datatype EA
	= Direct of register		(* %ri *)
	| Immed of int			(* immediate value, may be > 13-bits *)
	| ImmedLab of Label
      (* the following modes are for internal use only *)
	| Displace of (register * int)
	| Index of (register * register)
	| FloatReg of fregister

    datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

  (* global registers *)
    val g0 : register and g1 : register and g2 : register and g3 : register
    val g4 : register and g5 : register and g6 : register and g7 : register
  (* output registers *)
    val o0 : register and o1 : register and o2 : register and o3 : register
    val o4 : register and o5 : register and sp : register and o7 : register
  (* local registers *)
    val l0 : register and l1 : register and l2 : register and l3 : register
    val l4 : register and l5 : register and l6 : register and l7 : register
  (* input registers *)
    val i0 : register and i1 : register and i2 : register and i3 : register
    val i4 : register and i5 : register and fp : register and i7 : register

  (* floating-point registers *)
    val f0 : fregister and f1 : fregister and f2 : fregister and f3 : fregister

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

  (* generic data movement instruction (derived) *)
    val move : (EA * EA) -> unit

  (* load/store byte instructions *)
    val emit_ldb : (EA * EA) -> unit
    val emit_stb : (EA * EA) -> unit

  (* Span dependent psuedo-instructions for handling relative addresses. *)
    val loadAdr : (Label * int * register * register) -> unit
    val adjustAdr : (Label * register) -> unit

  (* control flow operations *)
    val emit_jmp : EA -> unit
    val emit_jmpl : EA -> unit

  (* conditional jumps (span dependent) *)
    val emitCondJmp : (condition * Label) -> unit
    val emitFCondJmp : (condition * Label) -> unit

  (* integer operations *)
    val emit_add : (EA * EA * EA) -> unit
    val emit_addcc : (EA * EA * EA) -> unit
    val emit_sub : (EA * EA * EA) -> unit
    val emit_sll : (EA * EA * EA) -> unit
    val emit_sra : (EA * EA * EA) -> unit
    val emit_and : (EA * EA * EA) -> unit
    val emit_or : (EA * EA * EA) -> unit
    val emit_xor : (EA * EA * EA) -> unit
    val emit_not : (EA * EA) -> unit
    val emit_cmp : (EA * EA) -> unit
    val emit_btst : (EA * EA) -> unit

    val emitIncr : (register * int) -> unit
    val emitDecr : (register * int) -> unit

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

end (* signature SPARCCODER *)
