(* mctrans.sml *)

(* translation of match code (mcexp) to FLINT plambda *)

structure TransMatch =
struct

(* numKey : Types.ty IntConst * T.ty -> key *)
(* Translates a front-end numeric literal (Types.ty IntConst.t) into a FLINT-style
 * numeric literal representation (int IntCons.t).
 * For compilation of matches to Absyn, we should stick withy front-end literals
 * and do the translation in FLINT/trans, so this function is not needed. *)
(* QUESTION: perhaps we should preserve the size, in the case of
 * word8, for better jump tables? [This applies to the translation to FLINT literals]. *)
fun transNum ({ival, ty}: T.ty IC.t) : int IC.t =
    let fun mkWORD sz = P.WORDcon{ival = v, ty = sz}  (* FLINT-style literal *)
	fun mkINT sz  = P.INTcon{ival = v, ty = sz}  (* FLINT-style literal *)
	val defaultIntSz = 63 (* = Target.defaultIntSz *)
     in if TU.equalType(ty, BT.intTy)
	  then mkINT defaultIntSz
	else if TU.equalType(ty, BT.int32Ty)
	  then mkINT 32
	else if TU.equalType(ty, BT.int64Ty)
	  then mkINT 64
	else if TU.equalType(ty, BT.intinfTy)
	  then mkINT 0
	else if TU.equalType(ty, BT.wordTy)
	  then mkWORD defaultIntSz
	else if TU.equalType(ty, BT.word8Ty)
	  then mkWORD defaultIntSz
	else if TU.equalType(ty, BT.word32Ty)
	  then mkWORD 32
        else if TU.equalType(ty, BT.word64Ty)
          then mkWORD 64
	else bug "transNum"
    end


fun transIntInf ... (genintinfswitch)

(* mkDcon : T.datacon -> P.dataconstr *)
fun mkDcon (DATACON {name, rep, typ, ...}) =
      (name, rep, toDconLty toLty typ)

(* keyToCon : MC.key * SV.svar option -> P.con *)
fun keyToCon(key,svarOp) =
    (case key
      of D dcon => 
	   let val svar = mkv()
	       val nts = ??? (* instantiation of polytype of dcon? / type args of dcon? *)
	    in P.DATAcon (mkDcon dcon, nts, newvar)
	   end
	 | V n => P.VLENcon n
	 | I num => transNum num
	 | W num => transNum num
	 | S s => P.STRINGcon s
	 | C c => P.INTcon (...))

(* mctrans : mcexp * ... -> Plambda.lexp *)
fun mctrans (Letr(svars,defsvar,body)) =
    let val deflvar = P.VAR(SV.svarLvar defsvar)
	fun trLetr (nil, _) = mctrans body
	  | trLetr (sv::rest, n) = 
	    P.LET(getLvar sv, P.SELECT(n, deflvar),
		  trLetr(rest, n+1))
     in trLetr(svars, 0)
    end

  | mctrans (Letr(svar, funexp, body)) = 
    P.LET(SV.svarLvar svar, mctrans funexp, mctrans body)

  | mctrans (Letm(vars, svars, body)) = 
    let fun trLetm (nil,nil) = mctrans body
	  | trLetm (v::restv, sv::restsv) = 
	    P.LET(V.varAccess v, P.VAR(SV.svarLvar sv),
		  trLetm(restv,restsv))
	  | trLetm _ = bug "mctrans: Letm"
    in trLetm(vars,svars)
    end
  | mctrans (Case(svar, cases, dflt)) =
    let fun trCase (key, svarOp, rhsexp) =
	    (keyToCon(key, svarOp), mctrans rhsexp)
		
  | mctrans (Sfun(vars, body)) = 
  | mctrans (Sapp(svar, argsvars)) = 
  | mctrans (Tfun(typevars, body)) = 

    
    P.LET(svarLvar(svar), P.RECORD(map (fn sv => P.VAR(getLvar(sv))) svars),
	  mctrans body)
