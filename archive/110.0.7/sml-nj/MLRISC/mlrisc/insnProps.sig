(* insnProps.sig --- instruction set properties
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)
signature INSN_PROPERTIES = sig
   structure C : CELLS
   structure I : INSTRUCTIONS

   sharing I.C = C

   datatype kind = IK_JUMP | IK_NOP | IK_INSTR
   datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

   val instrKind     : I.instruction -> kind
      (* kind of instruction  *)
   val moveInstr     : I.instruction -> bool
      (* is the instruction a move? Assumed to have exactly one
       * source and one destination 
       *)
   val branchTargets : I.instruction -> target list
      (* targets of an instruction. The instruction kind must be IK_JUMP *)
   val defUseR	     : I.instruction -> int list * int list
      (* general purpose registers def/use *)
   val defUseF	     : I.instruction -> int list * int list
      (* floating point register def/use *)
   val nop 	     : unit -> I.instruction
      (* generate a nop *)
   val latency 	: I.instruction -> int
      (* instruction latency *)
end


(*
 * $Log: insnProps.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:35  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/04/19 18:14:20  george
 *   Version 109.27
 *
 *)
