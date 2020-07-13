(* mctrans.sml *)

(* translation of match code (mcexp) to FLINT plambda *)

structure TransMatch =
struct

local
    structure T = Types
    structure BT = BasicTypes
    structure SV = SVar
    structure P = Plambda
    structure LT = PlambdaType
    open MCtypes
in
(* refers to : toLty, toTyc -- from PlambdaType? *)

(* transNum : Types.ty IntConst.t * T.ty -> int IntConst.t *)
(* Translates a front-end numeric literal (Types.ty IntConst.t) into a FLINT-style
 * numeric literal representation (int IntCons.t).
 * QUESTION: perhaps we should preserve the size, in the case of
 * word8, for better jump tables? *)
fun transNum ({ival, ty}: T.ty IntConst.t) : int IC.t =
    let fun mkWORD sz = P.WORDcon{ival = ival, ty = sz}  (* FLINT-style literal *)
	fun mkINT sz  = P.INTcon{ival = ival, ty = sz}  (* FLINT-style literal *)
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
	  then mkWORD defaultIntSz  (* or:  mkWORD 8 *)
	else if TU.equalType(ty, BT.word32Ty)
	  then mkWORD 32
        else if TU.equalType(ty, BT.word64Ty)
          then mkWORD 64
	else bug "transNum"
    end

fun transIntInf ... (genintinfswitch)
(** translating the typ field in DATACON into lty; constant datacons
    will take ltc_unit as the argument *)

fun toDconLty toLty ty =
    (case ty
      of TP.POLYty{sign, tyfun=TYFUN{arity, body}} =>
	   if BT.isArrowType body then toLty ty
	   else toLty (TP.POLYty{sign=sign,
				 tyfun=TYFUN{arity=arity,
					     body=BT.-->(BT.unitTy, body)}})
       | _ => if BT.isArrowType ty then toLty ty
	      else toLty (BT.-->(BT.unitTy, ty)))

(* mkDcon : T.datacon -> P.dataconstr *)
fun mkDcon (DATACON {name, rep, typ, ...}) =
      (name, rep, toDconLty toLty typ)

(* keyToCon : MC.key * SV.svar option -> P.con *)
(* In the case of V n, there will be a variable to be bound to
 * the vector contents? (a tuple? the whole vector?). In the
 * case of D dcon, there may or not be a variable, depending
 * on whether the constructor is a constant or not. *)
fun keyToCon(key,svarOp,ty) =
    (case key
      of D dcon =>
	   let lvar = (case svarOp
			of SOME sv = SV.svarLvar sv
			| NONE => LambdaVar.mkLvar())
	       val argtysV = TU.dataconInstArgs(ty,dcon)
	       val argtys = Vector.foldr (op ::) nil argtysV
	       val argtycs = map toTyc argtys  (* toTyc? where from? *)
		   (* instantiation of polytype of dcon? / type args of dcon?
                    * translated to FLINT tycs *)
	    in P.DATAcon (mkDcon dcon, argtycs, lvar)
	   end
	 | V n => P.VLENcon n      (* svarOp = SOME v *)
	 | I num => transNum num   (* svarOp = NONE *)
	 | W num => transNum num   (* svarOp = NONE *)
	 | S s => P.STRINGcon s    (* svarOp = NONE *)
	 | C c => P.INTcon (...))  (* svarOp = NONE *)


fun keyConsig ((key,_,_)::_) =
    (case key
      of D dcon => TU.dataconSign dcon
       | _ => Access.CNIL)
  | keyConsig nil = bug "keyConsig"

(* mctrans : mcexp * ... -> Plambda.lexp *)
fun mctrans (Letr(svars,defsvar,body), toLty) =
    let val deflvar = P.VAR(SV.svarLvar defsvar)
	fun trLetr (nil, _) = mctrans body
	  | trLetr (sv::rest, n) =
	    P.LET(getLvar sv, P.SELECT(n, deflvar),
		  trLetr(rest, n+1))
     in trLetr(svars, 0)
    end

  | mctrans (Letf(svar, funexp, body)) =
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
    let val scrutineeTy = SV.svarType svar
	fun trCase (key, svarOp, rhsexp) =
	    (keyToCon(key, svarOp, ty), mctrans rhsexp)
    in P.SWITCH(P.VAR(SV.svnLvar svar), keyConsig cases, map trCase cases,
		Option.map mctrans dflt)
    end

  | mctrans (Sfun(vars, body)) =
    let val lbody = mctrans body
	fun varToLvar (VALvar{access=DA.LVAR v, typ,...}) = (v, toLty (!typ))
	  | varToLvar _ = bug "varToLvar - bad variable"
	fun transFun ([], lexp) = FN(LV.mkLvar(), LT.ltc_unit, lexp)
          | transFun ([v], lexp) =
            let val (argvar,argt) = varToLvar v
             in FN(argvar,argt,lexp)
            end
          | transFun (vl, lexp) =
            let val argvar = LV.mkLvar()
                fun transArgs (nil, n) = (lexp,nil)
                  | transArgs (var::vars, n) =
                      let val (lv,lty) = varToLvar var
                          val (lexp',ltys) = transArgs(vars,n+1)
                       in (LET(lv, SELECT(n, VAR argvar), lexp'), lty :: ltys)
                      end
                val (funbody,ltys) = transArgs(vl,0)
             in P.FN(argvar, LT.ltc_tuple ltys, funbody)
            end
     in transFun (vars, lbody)
    end

  | mctrans (Sapp(svar, argsvars)) =
      P.APP(P.VAR(SV.svarLvar svar),
	    P.RECORD(map (fn sv => P.VAR(SV.svarLvar sv)) argsvars))

  | mctrans (Tfun(typevars, body)) =
    let val tkinds = map (fn tv => LT.tkc_mono) typevars
     in P.TFN(tkinds, mctrans body)
    end

end (* local *)
end (* TransMatch *)
