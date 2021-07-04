(* tvarcvt.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * converting between different representations of
 * type variables in a FLINT program.
 *)

signature TVARCVT =
sig
  val debIndex2names : FLINT.prog -> FLINT.prog
  val names2debIndex : FLINT.prog -> FLINT.prog
end (* signature TVARCVT *)

structure TvarCvt :> TVARCVT =
struct

    (* local abbreviations *)
    structure F = FLINT
    structure DI = DebIndex
    structure LV = LambdaVar
    structure LE = LtyExtern
    structure LK = LtyKernel

    structure PP= PrettyPrint
    val with_pp = PP.with_default_pp

    fun ppTyc tyc =
      with_pp (fn ppstm => (PPLty.ppTyc 20 ppstm tyc))
    fun ppLty lty =
      with_pp (fn ppstm => (PPLty.ppLty 20 ppstm lty))

    val wrdebugging = FLINT_Control.wrdebugging
		      
    val say = Control.Print.say
    fun dbsay msg = if !wrdebugging then say msg else ()
    fun bug msg = ErrorMsg.impossible("TvarCvt: " ^ msg)

    type depth = DebIndex.depth (* = int; >= 0 *)

    (* `debIndex2names' converts all variables bound by the
     * term-language TFN (capital lambda) construct into named
     * variables. This is primarily to experiment with the cost of
     * named variables, should we introduce them during translate or
     * other phases.
     *)
    val debIndex2names =
	let fun extendEnv (env, tvtks) =
                let val (tvars,ks) = ListPair.unzip tvtks
		in Lty.teCons(Lty.Beta (0, map LE.tcc_nvar tvars, ks), env)
		end

            fun cvtExp (env: Lty.tycEnv, d: depth, lexp: F.lexp) =
		let fun tcSubst tyc = LK.tcc_env (tyc, d, d, env)
		    fun ltSubst lty = LK.ltc_env (lty, d, d, env)

		    fun cvtCon (F.DATAcon ((sym,cr,lty),ts,lv)) =
			  F.DATAcon ((sym, cr, ltSubst lty),
				     map tcSubst ts, lv)
		      | cvtCon c = c

		    fun cvtDict {default, table} : F.dict =
			let fun f (ts,lv) = ((map tcSubst ts), lv)
			in {default = default, table = map f table}
			end (* cvtDict *)

		    fun cvtPrimop (dictOpt, po, lty, tycs) : F.primop =
			(Option.map cvtDict dictOpt, po, ltSubst lty,
			 map tcSubst tycs)

		    fun cvt exp =
			case exp
			 of F.RET _ => exp              (* no processing required *)

			  | F.LET (lvs, e1, e2) =>      (* recursion only *)
			    F.LET (lvs, cvt e1, cvt e2)

			  | F.FIX (fundecs, e) =>       (* recursion only *)
			    F.FIX (map (cvtFundec env d) fundecs,
				   cvt e)

			  | F.APP _ => exp              (* no processing required *)

			  | F.TFN ((tfk, v, tvtks, e1), e2) =>
			    F.TFN ((tfk, v, tvtks,
				    cvtExp (extendEnv (env, tvtks), d+1, e1)),
				   cvt e2)

			  | F.TAPP (v, ts) =>           (* subst ts *)
			    F.TAPP (v, map tcSubst ts)

			  | F.SWITCH (v, cs, conlexps, lexpOp) =>
			    F.SWITCH (v, cs,
				      (map (fn (con,lexp) => (cvtCon con, cvt lexp))
					   conlexps),
				      Option.map cvt lexpOp)

			  | F.CON ((sym,cr,lty), ts, v, lv, e) =>
			    F.CON ((sym, cr, ltSubst lty), map tcSubst ts, v, lv, cvt e)

			  | F.RECORD (rk, vs, lv, e) =>
			    F.RECORD ((case rk of
					   F.RK_VECTOR t =>
					   F.RK_VECTOR (tcSubst t)
					 | _ => rk),
				      vs, lv, cvt e)

			  | F.SELECT (v, i, lv, e) =>
			    F.SELECT (v, i, lv, cvt e)

			  | F.RAISE (v, ltys) =>
			    F.RAISE (v, map ltSubst ltys)

			  | F.HANDLE (e, v) =>
			    F.HANDLE (cvt e, v)

			  | F.BRANCH (po, vs, e1, e2) =>
			    F.BRANCH (cvtPrimop po,
				      vs, cvt e1, cvt e2)

			  | F.PRIMOP (po, vs, lv, e) =>
			    F.PRIMOP (cvtPrimop po,
				      vs, lv, cvt e)
		in cvt lexp
		end (* fun cvtExp *)

            and cvtFundec env d (fkind, lvar, lvlts, e) =
		let fun ltSubst lty = LK.ltc_env (lty, d, d, env)

		    fun cvtFkind {isrec = SOME(ltys,lk),
				  cconv, known, inline} =
			{isrec = SOME (map ltSubst ltys, lk),
			 cconv = cconv,
			 known = known,
			 inline = inline}
		      | cvtFkind fk = fk

		    fun cvtLvLt (lvar, lty) = (lvar, ltSubst lty)
		in (cvtFkind fkind, lvar, map cvtLvLt lvlts, cvtExp (env, d, e))
		end (* cvtFundec *)

	in cvtFundec Lty.teEmpty DI.top
	end

    (* env: an environment used dor the names2debIndex conversion *)
    type env = (int * int) LV.Map.map

    (* `names2debIndex' removes all named variables (`TC_NVAR')
     * from a FLINT program, replacing them with deBruijn-indexed
     * variables.  It expects, of course, that named variables are
     * only bound by the term-language TFN (capital lambda), and not
     * by the LT_POLY (forall) or TC_FN (lowercase lambda) in the
     * type language.
     *)
    fun names2debIndex prog =
    let
	(* insertTvars : env * depth * (tvar * tkind) list -> env *)
        (* tkinds are ignored
	 * binderDepth : TFN-binding depth;
	 * i : tv index within n-ary TFN binding, i.e. within (tvar * tkind) list *)
        fun insertTvars (env, binderDepth, tvtks) =
	    let fun addTvars (nil, i, env) = env
	          | addTvars ((tv,_)::rest, i, env) = (* ignoring tkind *)
		      addTvars (rest, i+1, LV.Map.insert (env, tv, (binderDepth,i)))
	     in addTvars (tvtks, 0, env)
	    end

        (* relativeDepth : int * int -> int
         * REQUIRES: cur > def
         * ASSERT: return value > 0 *)
        fun relativeDepth (cur: int, def: int): int =
	    if cur > def then cur - def
	    else bug "relativeDepth"

        (* findDTvar : env -> (tvar * depth) -> Lty.tyc option
         * ASSERT: result is hashed TC_VAR(dbindex,i) *)
        fun findDTvar env (tvar, currDepth) =
	   (case LV.Map.find (env, tvar)
	      of NONE => (dbsay ("findDTvar: not found: " ^ LV.prLvar tvar ^ "\n"); NONE)
	       | SOME (binderDepth, i) =>
		  (dbsay ("findDTvar: found: " ^ LV.prLvar tvar ^ "\n");
	           SOME (LE.tcc_var (relativeDepth (currDepth, binderDepth), i)))
           (*esac*))

        (* tc_nvar_elim: (tvar * DebIndex.depth -> tyc option)
                         -> DebIndex.depth -> tyc -> tyc
           lt_nvar_elim: (tvar * DebIndex.depth -> tyc option)
                         -> DebIndex.depth -> lty -> lty*)
        val tc_nvar_elim = LE.tc_nvar_elim_gen ()
        val lt_nvar_elim = LE.lt_nvar_elim_gen ()

        (* cvtExp : env * depth * F.lexp -> F.lexp *)
        fun cvtExp (env, depth, lexp) =
	    let val find = findDTvar env
		(* instantiate a new subst dictionary on each invocation..
		 * clean this up later. HOW??? *)
		val tcSubst : LE.tyc -> LE.tyc = tc_nvar_elim find depth
		val ltSubst : LE.lty -> LE.lty = lt_nvar_elim find depth

		fun cvtCon (F.DATAcon ((sym, cr, lty), ts, lv)) =
		    F.DATAcon ((sym, cr, ltSubst lty), map tcSubst ts, lv)
		  | cvtCon c = c

		fun cvtDict ({default, table} : F.dict) : F.dict =
		    let fun f (ts,lv) = ((map tcSubst ts), lv)
		     in {default = default, table = map f table}
		    end (* cvtDict *)

		fun cvtPrimop ((dictOpt, po, lty, tycs) : F.primop) : F.primop =
		    let (* val _ = dbsay ("@@@ cvtPrimop: "^PrimopUtil.toString po^"\n") *)
			val lty' = ltSubst lty
(*			val _ = (dbsay "  @@@ lty = "; ppLty lty)
			val _ = (dbsay "  @@@ lty' = "; ppLty lty')
 *)
		    in (Option.map cvtDict dictOpt, po, lty', map tcSubst tycs)
		    end

		fun cvt exp =                 (* default recursive invocation *)
		    case exp
		     of F.RET _ => exp              (* no processing required *)

		      | F.LET (lvs, e1, e2) =>      (* recursion only *)
			F.LET (lvs, cvt e1, cvt e2)

		      | F.FIX (fundecs, e) =>       (* recursion only *)
			F.FIX (map (cvtFundec (env, depth)) fundecs, cvt e)

		      | F.APP _ => exp              (* no processing required *)

		      | F.TFN ((tfk, v, tvtks, e1), e2) =>
			F.TFN ((tfk, v, tvtks,
				cvtExp (insertTvars(env, depth, tvtks), depth+1, e1)),
			       cvt e2)

		      | F.TAPP (v, ts) =>           (* subst ts *)
			F.TAPP (v, map tcSubst ts)

		      | F.SWITCH (v, cs, conlexps, lexpO) =>
			F.SWITCH (v, cs,
				  map (fn (con,lexp) => (cvtCon con, cvt lexp))
				      conlexps,
				  Option.map cvt lexpO)

		      | F.CON ((sym,cr,lty), ts, v, lv, e) =>
			F.CON ((sym, cr, ltSubst lty),
			       map tcSubst ts,
			       v, lv, cvt e)

		      | F.RECORD (rk, vs, lv, e) =>
			F.RECORD ((case rk of
				       F.RK_VECTOR t =>
				       F.RK_VECTOR (tcSubst t)
				     | _ => rk),
				  vs, lv, cvt e)

		      | F.SELECT (v, i, lv, e) =>
			F.SELECT (v, i, lv, cvt e)

		      | F.RAISE (v, ltys) =>
			F.RAISE (v, map ltSubst ltys)

		      | F.HANDLE (e, v) =>
			F.HANDLE (cvt e, v)

		      | F.BRANCH (po, vs, e1, e2) =>
			F.BRANCH (cvtPrimop po,
				  vs, cvt e1, cvt e2)

		      | F.PRIMOP (po, vs, lv, e) =>
			F.PRIMOP (cvtPrimop po,
				  vs, lv, cvt e)
	     in cvt lexp
	    end (* cvtExp *)

        (* cvtFundec : env * depth -> prog -> prog *)
        and cvtFundec (env, depth) ((fkind, lvar, params, body) : F.fundec) =
            let val _ = dbsay ("@@@ cvtFundec: " ^ LV.lvarName lvar ^ "\n")
		val find = findDTvar env
                (* instantiate a new subst dictionary on each invocation..
                 * clean this up later. *)
		val tcSubst = tc_nvar_elim find depth
		val ltSubst = lt_nvar_elim find depth

		fun cvtFkind ({isrec = SOME (ltys,lk),
			       cconv, known, inline}) =
                    {isrec = SOME (map ltSubst ltys, lk),
		     cconv = cconv,
		     known = known,
		     inline = inline}
		  | cvtFkind fk = fk

		fun cvtParam (lvar, lty) = (lvar, ltSubst lty)
					      
             in (cvtFkind fkind, lvar, map cvtParam params, cvtExp(env, depth, body))
            end (* cvtFundec *)

     (* An env (table) is generated on each invocation, ie, once per compilation unit. *)

     in cvtFundec (LV.Map.empty, DI.top) prog
    end (* names2debIndex *)

end (* structure TvarCvt *)
