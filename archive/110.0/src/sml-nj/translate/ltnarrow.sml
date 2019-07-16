(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* ltnarrow.sml *)

signature LTNARROW = 
sig
  val narrow : Lambda.lexp -> Lambda.lexp
end (* signature LTYCOMP *)

structure LtNarrow : LTNARROW = 
struct

local structure LP = LtyPrim
      structure LE = LtyEnv
      open Lambda
in

fun bug s = ErrorMsg.impossible ("LtNarrow: " ^ s)

fun narrow lexp = 
let

fun lpsv sv = 
  (case sv
    of VAR v => sv
     | (INT _ | WORD _ | INT32 _ | WORD32 _ | REAL _ | STRING _) => sv
     | PRIM (p, t, []) => PRIM(p, LE.ltNarrow t, [])
     | PRIM _ => bug "unexpected PRIM in loop"
     | GENOP _ => bug "unexpected GENOP in loop")

fun loop le = 
  (case le
    of SVAL sv => SVAL(lpsv sv)
     | TFN (ks, e) => bug "unexpected TFN in loop"
     | TAPP (v, ts) => bug "unexpected TAPP in loop"

     | WRAP(tc, b, v) => WRAP(LE.tcNarrow tc, b, lpsv v)
     | UNWRAP(tc, b, v) => UNWRAP(LE.tcNarrow tc, b, lpsv v)

     | CON _ => bug "unexpected CON in loop"
     | DECON _ => bug "unexpected CON in loop"

     | SWITCH (v, reps, cases, opp) => 
         let fun g (c, x) = (c, loop x)
             fun h (NONE) = NONE
               | h (SOME x) = SOME(loop x)
          in SWITCH(lpsv v, reps, map g cases, h opp)
         end

     | FN(v, t, e) => FN(v, LE.ltNarrow t, loop e)
     | FIX(vs, ts, es, eb) => 
         FIX(vs, map LE.ltNarrow ts, map loop es, loop eb)
     | APP(v1, v2) => APP(lpsv v1, lpsv v2)
     | LET(v, e1, e2) => LET(v, loop e1, loop e2)
     | RECORD vs => RECORD(map lpsv vs)
     | SRECORD vs => SRECORD(map lpsv vs)
     | VECTOR (vs, t) => VECTOR(map lpsv vs, LE.tcNarrow t)   
     | SELECT (i, v) => SELECT(i, lpsv v)

     (* I'd like to make the following concrete in the future *)
     | EXNF (v, t) => EXNF(lpsv v, LE.ltNarrow t) 
                                           (* t is always monomorphic *)
     | EXNC v => EXNC(lpsv v)

     | RAISE (v, t) => RAISE(lpsv v, LE.ltNarrow t)       
                                           (* t is always monomorphic *)
     | HANDLE (e, v) => HANDLE(loop e, lpsv v)

     | PACK _ => bug "unexpected PACK lexp in ltyComp")

in loop lexp
end (* function narrow *)
end (* toplevel local *)
end (* structure LtNarrow *)


(*
 * $Log: ltnarrow.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:48  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/05/05 20:00:11  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:46  george
 *   Version 109.24
 *
 *)
