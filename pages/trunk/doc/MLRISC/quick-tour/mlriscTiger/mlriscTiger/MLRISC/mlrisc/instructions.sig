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

    type ea
    type operand    
    type instruction
end


(*
 * $Log$
 * Revision 1.1  2001/10/11 09:52:26  macqueen
 * Initial revision
 *
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
