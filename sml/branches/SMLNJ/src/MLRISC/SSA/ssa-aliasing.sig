(*
 * Some tentative SSA aliasing information to be provided by the frontend.
 *)
signature SSA_MEMORY_ALIASING =
sig

   structure Region : REGION

     (*
      * Action associated with a region
      *)
   datatype action = 
     INIT_RO   (* initialize read-only region *)
   | INIT      (* initialize read/write region *)
   | READ_RO   (* read from read only region *)
   | READ      (* read from read/write region *)
   | WRITE     (* write to read/write region *)

   val readAction   : Region.region -> action
   val writeAction  : Region.region -> action
   val readRegions  : Region.region -> int list (* uses *)
   val writeRegions : Region.region -> int list * int list (* defs / uses *)

end

(*
 * $Log: ssa-aliasing.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:47:06  george
 *  Version 110.10
 *
 *)
