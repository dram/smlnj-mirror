(* transRVB.sml *)
(* fragments of code relating to translating RVB declarations from 110.98.1 *)

(* ================================================================================ *)
(* from ElabUtil *)

fun wrapRECdecGen (rvbs, compInfo as {mkLvar=mkv, ...} : compInfo) =
  let fun g (RVB{var=v as VALvar{path=SP.SPATH [sym], ...}, ...}, nvars) =
          let val nv = newVALvar(sym, mkv)
          in ((v, nv, sym)::nvars)
          end
	| g _ = bug "wrapRECdecGen:RVB"
      val vars = foldr g [] rvbs
      val odec = VALRECdec rvbs

      val tyvars =
        case rvbs
         of (RVB{tyvars,...})::_ => tyvars
          | _ => bug "unexpected empty rvbs list in wrapRECdecGen"

   in (vars,
       case vars
        of [(v, nv, sym)] =>
            (VALdec [VB{pat=VARpat nv, boundtvs=[], tyvars=tyvars,
                        exp=LETexp(odec, VARexp(ref v, []))}])
         | _ =>
          (let val vs = map (fn (v, _, _) => VARexp(ref v, [])) vars
               val rootv = newVALvar(internalSym, mkv)
               val rvexp = VARexp(ref rootv, [])
               val nvdec =
                 VALdec([VB{pat=VARpat rootv, boundtvs=[], tyvars=tyvars,
                            exp=LETexp(odec, TUPLEexp vs)}])

               fun h([], _, d) =
                     LOCALdec(nvdec, SEQdec(rev d))
                 | h((_,nv,_)::r, i, d) =
                     let val nvb = VB{pat=VARpat nv, boundtvs=[],
                                      exp=TPSELexp(rvexp,i),tyvars=ref []}
                      in h(r, i+1, VALdec([nvb])::d)
                     end
            in h(vars, 1, [])
           end))
  end

fun wrapRECdec (rvbs, compInfo) =
  let val (vars, ndec) = wrapRECdecGen(rvbs, compInfo)
      fun h((v, nv, sym), env) = SE.bind(sym, B.VALbind nv, env)
      val nenv = foldl h SE.empty vars
   in (ndec, nenv)
  end


(* ================================================================================ *)
(* from translate.sml *)

and mkVBs (vbs, d) =
  let fun mkVB (VB{pat,exp,boundtvs,...}, body) =
	  case AbsynUtil.stripPatMarks pat
            of (VARpat(V.VALvar{access=DA.LVAR v, ...}) |
                CONSTRAINTpat(VARpat(V.VALvar{access=DA.LVAR v, ...}),_)) =>
                  (* simple variable pattern: No special case needed for primops [dbm: 5/1/07] *)
                  LET(v, mkPE(exp, d, boundtvs), body)

              | pat =>
		(* boundtvs is cumulative bound univariables for the whole pattern *)
		let val (newpat,oldvars,newvars) = aconvertPat (pat, compInfo)
		      (* this is the only call of aconvertPat; it replaces pattern variables with
		       * new versions with fresh lvar access values *)
		    val newVarExps = map (fn v => VARexp(ref v,[])) newvars
		    val rhsTy = BasicTypes.tupleTy(map (fn (V.VALvar{typ,...}) => !typ) newvars)
		    val bindRule = RULE(newpat, EU.TUPLEexp(newVarExps))
		    val defaultRule = RULE(WILDpat,
					   RAISEexp(CONexp(CoreAccess.getExn env ["Bind"],[]),rhsTy))
		    val newexp = CASEexp(exp, [bindRule, defaultRule], false)

		 in case oldvars
		     of [] => (* variable-free pattern, implies boundtvs = [], hence no type abs *)
			  LET(mkv(), mkExp(newexp, d), body) (* fresh let-bound lvar doesn't occur in body *)
		      | _ =>
			let val newVar = mkv() (* new local variable to be let-bound to newexp *)
			    fun lookup (tv: Types.tyvar) [] = NONE
			      | lookup tv ((tv',k)::r) = if tv = tv' then SOME k
							 else lookup tv r
			    fun buildDec([], i, body) = body
			      | buildDec(bvar::rest, i, body) =
				let val V.VALvar{access=DA.LVAR(lv),btvs,...} = bvar
				    val btvs = !btvs
				    (* bound univariables for this particular pattern variable
				       btvs is a subset of boundtvs -- possibly proper *)
				    val tvarity = length(btvs)
				    val defn = case (boundtvs, btvs)
						of ([],[]) =>
						   SELECT(i,VAR(newVar))
						 | (_,[]) =>
						   SELECT(i,TAPP(VAR(newVar),
								 map (fn _ => LT.tcc_void) boundtvs))
						 | _ =>
						   let val indices = List.tabulate(tvarity, (fn x => x))
						       (* 0-based index into bound type variable sequence *)
						       val tvToIndex = ListPair.zip(btvs,indices)
						       val targs = map (fn tv => case lookup tv tvToIndex
										  of NONE => LT.tcc_void
										   | SOME k => LT.tcc_var(1,k))
								       boundtvs
						   in TFN(LT.tkc_arg(tvarity),
							  SELECT(i,TAPP(VAR(newVar),targs)))
						   end
				 in buildDec(rest,i+1,LET(lv, defn, body))
				end

			   in LET(newVar,mkPE(newexp,d,boundtvs),
				  buildDec(oldvars, 0, body))
			  end
		end

   in fold mkVB vbs
  end (* mkVBs *)

and mkRVBs (rvbs, d) =
  let fun mkRVB (RVB{var=V.VALvar{access=DA.LVAR v, typ=ref ty, ...},
                     exp, boundtvs, ...}, (vlist, tlist, elist)) =
            let val ee = mkExp(exp, d) (* was mkPE(exp, d, boundtvs) *)
                val vt = toLty d ty
            in (v::vlist, vt::tlist, ee::elist)
            end
        | mkRVB _ = bug "unexpected valrec bindings in mkRVBs"

      val (vlist, tlist, elist) = foldr mkRVB ([], [], []) rvbs

   in fn b => FIX(vlist, tlist, elist, b)
  end
      

(* ================================================================================ *)
(* from typecheck.sml *)

and decType0(decl,occ,tdepth,region) : dec =
     case decl
       of VALdec ... => ...
        | VALRECdec(rvbs) =>
 	   let val occ = Abstr occ

	       (* First go through and type-check all the patterns and
		  result-constraints, unifying with each other and with
		  the specified result type.
	       *)
	       fun setType(rvb as RVB{var=VALvar{typ,...},exp,resultty,...}) =
                   let val domainty = mkMETAtyBounded(lamdepth occ)
		       val rangety = mkMETAtyBounded(lamdepth occ)
                                      (* depth should be infinity? *)
		       val funty = domainty --> rangety

		       val _ =
			   case resultty
			     of NONE => true
			      | SOME ty =>
				 unifyErr{ty1=funty,ty2=ty,
					  name1="",name2="constraint",
					  message="type constraint of val rec dec\
					           \ is not a function type",
					  region=region,kind=ppRVB,
					  kindname="declaration", phrase=rvb}

		       fun f(FNexp(rules,_), region, funty) =
		             let fun unify a =
				  (unifyErr{ty1=a,name1="this clause",
				    ty2=funty,name2="previous clauses",
				    message="parameter or result constraints\
			                     \ of clauses do not agree",
					   region=region,kind=ppRVB,
					   kindname="declaration", phrase=rvb};
                                  ())

				 fun approxRuleTy(RULE(pat,e)) =
				     let val (pat',pty) =
					     patType(pat,lamdepth occ,region)
				      in case e
					  of CONSTRAINTexp(e,ty) =>
					      (pat',pty-->ty,(e,region))
					   | e => (pat',pty-->rangety,(e,region))
				     end

				 val patTyExps = map approxRuleTy rules
				 val pats = map #1 patTyExps
				 val tys = map #2 patTyExps
				 val exps = map #3 patTyExps

				 fun doExp (e,region) =
				     let val (exp', ety) = expType(e,occ,tdepth,region)
				      in unifyErr{ty1=ety, name1="expression",
					  ty2=rangety, name2="result type",
					  message="right-hand-side of clause\
					\ does not agree with function result type",
					  region=region,kind=ppRVB,
					  kindname="declaration",phrase=rvb};
					 exp'
				     end

                              in app unify tys;
				 typ := funty;
				 fn()=>
				   FNexp(ListPair.map RULE (pats, map doExp exps),
						domain(prune(funty)))
			     end
		         | f(MARKexp(e,region),_,funty) =
			     let val build = f(e,region,funty)
			      in fn()=> MARKexp(build(), region)
			     end
                         | f(CONSTRAINTexp(e,ty),region,funty) =
			     let val _ =
				   unifyErr{ty1=ty, name1="this constraint",
					    ty2=funty, name2="outer constraints",
					    message="type constraints on val rec\
					             \ declaraction disagree",
					    region=region,kind=ppRVB,
					    kindname="declaration", phrase=rvb}
				 val build = f(e,region,funty)
			     in fn()=> CONSTRAINTexp(build(), ty)
			    end
			| f _ = bug "typecheck.823"
                   in f(exp,region,funty)
                  end
		 | setType _ = bug "setType"

	      (* Second, go through and type-check the right-hand-side
	         expressions (function bodies) *)
	       fun rvbType(RVB{var=v,resultty,tyvars,boundtvs,...}, build) =
                      RVB{var=v,exp=build(), resultty=resultty,tyvars=tyvars,
			  boundtvs=boundtvs}

	       val _ = debugmsg ">>decType0: VALRECdec"
               val builders = map setType rvbs
               val rvbs' = ListPair.map rvbType (rvbs,builders)
               (* No need to generalize here, because every VALRECdec is
                  wrapped in a VALdec, and the generalization occurs at the
                  outer level.  Previously: val rvbs'' = map genType rvbs' *)
	    in EU.recDecs rvbs'
	   end
