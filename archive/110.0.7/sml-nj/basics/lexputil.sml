(* Copyright 1996 by AT&T Bell Laboratories *)
(* lexputil.sml *)

signature LEXPUTIL =
sig
  val copy : (unit -> LambdaVar.lvar) -> Lambda.lexp -> Lambda.lexp
end (* signature LEXPUTIL *) 


structure LexpUtil : LEXPUTIL = 
struct

local structure EM = ErrorMsg
      open Access Lambda
in 

fun bug msg = EM.impossible("LexpUtil: "^msg)

(*
 *   Small modification to the alpha-converter (which was also introduced
 *   by myself much earlier).  It no longer requires the input expression
 *   to be closed.  Free variables are left alone.  Furthermore, I fixed a
 *   bug related to exception constructors -- those have lambda variables
 *   inside the access field, and those need to be alpha-renamed as well.
 * 						-- m.blume
 *)

(* general alpha-conversion on lexp
 * free variables remain unchanged
 * val copy: (unit -> lvar) -> lexp -> lexp *)
fun copy mkLvar = let

    fun look m v = (IntmapF.lookup m v) handle IntmapF.IntmapF => v

    fun rename (lv, m) = let
	val lv' = mkLvar ()
	val m' = IntmapF.add (m, lv, lv')
    in
	(lv', m')
    end

    (* access *)
    fun ca (LVAR v, m) = LVAR (look m v)
      | ca (PATH (a, i), m) = PATH (ca (a, m), i)
      | ca (a, _) = a

    (* conrep *)
    fun ccr (EXNFUN a, m) = EXNFUN (ca (a, m))
      | ccr (EXNCONST a, m) = EXNCONST (ca (a, m))
      | ccr (cr, _) = cr

    (* con *)
    fun ccon (DATAcon (s, cr, t), m) = DATAcon (s, ccr (cr, m), t)
      | ccon (con, _) = con

    (* dataconstr *)
    fun cdc ((s, cr, t), m) = (s, ccr (cr, m), t)

    fun dict ({default=v, table=tbls}, m) =
      let val nv = look m v
          val ntbls = map (fn (x, v) => (x, look m v)) tbls
       in {default=nv, table=ntbls}
      end

    (* value *)
    fun sv (VAR lv, m) = VAR (look m lv)
      | sv (x as INT _, _) = x
      | sv (x as INT32 _, _) = x
      | sv (x as WORD _, _) = x
      | sv (x as WORD32 _, _) = x
      | sv (x as REAL _, _) = x
      | sv (x as STRING _, _) = x
      | sv (x as PRIM _, _) = x       
      | sv (x as GENOP(dt,p,t,ts), m) = GENOP(dict(dt,m), p, t, ts)

    (* lexp *)
    fun c (SVAL v, m) = SVAL (sv(v, m))
      | c (FN (lv, t, e), m) = 
          let val (lv', m') = rename (lv, m)
   	   in FN (lv', t, c (e, m'))
   	  end
      | c (APP (v1, v2), m) = APP (sv (v1, m), sv (v2, m))

      | c (FIX (lvl, ltl, el, e), m) = let
	    fun ren1 (lv, (lvl, m)) = let
		val (lv', m') = rename (lv, m)
	    in
		(lv' :: lvl, m')
	    end
	    val (lvl', m') = foldr ren1 ([], m) lvl
	    fun c' x = c (x, m')
	in
	    FIX (lvl', ltl, map c' el, c' e)
	end
      | c (SWITCH (v, crl, cel, eo), m) = let
	    fun cc (con, x) = (ccon (con, m), c (x, m))
	    fun co NONE = NONE
	      | co (SOME x) = SOME (c (x, m))
	in
	    SWITCH (sv (v, m),
		    crl,
		    map cc cel,
		    co eo)
	end
      | c (CON (dc, ts, v), m) = CON (cdc (dc, m), ts, sv (v, m))
      | c (DECON (dc, ts, v), m) = DECON (cdc (dc, m), ts, sv (v, m))
      | c (VECTOR (vl, t), m) = VECTOR (map (fn x => sv (x, m)) vl, t)
      | c (RECORD vl, m) = RECORD (map (fn x => sv (x, m)) vl)
      | c (SRECORD vl, m) = SRECORD (map (fn x => sv (x, m)) vl)
      | c (SELECT (i, v), m) = SELECT (i, sv (v, m))
      | c (EXNF (v, t), m) = EXNF (sv (v, m), t)
      | c (EXNC v, m) = EXNC (sv (v, m))
      | c (RAISE (v, t), m) = RAISE (sv (v, m), t)
      | c (HANDLE (e, v), m) = HANDLE (c (e, m), sv (v, m))
      | c (WRAP (t, b, v), m) = WRAP (t, b, sv (v, m))
      | c (UNWRAP (t, b, v), m) = UNWRAP (t, b, sv (v, m))
      | c _ = bug "unsupported lambda expression"
in
    fn e => c (e, IntmapF.empty)
end

end (* top-level local *)
end (* structure LexpUtil *)

(*
 * $Log: lexputil.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/05/05 19:59:39  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:10  george
 *   Version 109.24
 *
 *)
