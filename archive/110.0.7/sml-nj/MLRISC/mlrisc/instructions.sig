(* instruction.sig --- target machine instructions 
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* basically says: 
 * represent instructions any which way you want 
 *)
signature INSTRUCTIONS = sig
    structure C : CELLS
    structure Constant : CONSTANT

    type operand    
    type instruction
end


(*
 * $Log: instructions.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:35  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/04/19 18:14:20  george
 *   Version 109.27
 *
 *)
