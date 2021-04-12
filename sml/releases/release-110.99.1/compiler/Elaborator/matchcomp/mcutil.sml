(* ElabData/matchcomp/mcutil.sml *)

structure MCUtil =
struct

local
  structure S = Symbol
  structure A = Access
  structure LV = LambdaVar
  structure V = VarCon
  structure AS = Absyn
  open Absyn MCTypes

  fun bug msg = ErrorMsg.impossible ("MCUtil: "^ msg)

in

(* debugging print function *)
fun printMappedVars (var, svar, header) =
    let val varLvar =
	    case V.varAccess var
	      of A.LVAR lv => lv
	       | _ => bug "printMappedVars 0"
        val svarLvar =
	    case V.varAccess svar
	      of A.LVAR lv => lv
	       | _ => bug "printMappedVars 1"
    in print (concat [header, Symbol.name (V.varName var), "[", LambdaVar.prLvar varLvar,
		      "] ==> ", Symbol.name (V.varName svar), "[",
		      LambdaVar.prLvar svarLvar, "]\n"])
    end
(*
(* substVars : AS.exp * varmap -> unit *)
fun substVars (exp: AS.exp, varmap: (V.var * SVar.svar) list) =
    let fun lookup (var: V.var) =
	    let fun look nil = NONE
		  | look ((v,sv)::rest) =
		    if V.eqVar(v,var)
		    then (printMappedVars (var, sv, "L: "); SOME sv) 
		    else look rest
	     in look varmap
	    end
	fun substExp exp =
            (case exp
	      of VARexp (varref, _) =>
		   if V.hasLvarAccess (!varref)
		   then (case lookup (!varref)
			   of SOME sv => varref := SVar.svarToVar sv
				(* replace the var with the associated svar *)
			    | NONE => ())
		   else ()
	      | RECORDexp fields => 
		  List.app (fn (l,e) => substExp e) fields
              | VECTORexp (exps, _) =>
		  List.app substExp exps
	      | APPexp (rator, rand) => 
		  (substExp rator; substExp rand)
	      | FNexp (rules,_,_) =>  (* single simple rule after transMatch *)
		  List.app (fn (RULE(p,e)) => substExp e) rules
	      | HANDLEexp (base, (rules,_,_)) => (* single simple rule after transMatch *)
		  (substExp base; List.app (fn (RULE(p,e)) => substExp e) rules)
	      | CASEexp (scrut, (rules,_,_)) => (* transMatch will have eliminated these *)
	          (substExp scrut; List.app (fn (RULE(p,e)) => substExp e) rules)
	      | SWITCHexp (_, rules, defaultOp) => 
		  (List.app (fn RULE(p,e) => substExp e) rules;
		   Option.app substExp defaultOp)
	      | VSWITCHexp (_, rules, default) => 
		  (List.app (fn RULE(p,e) => substExp e) rules;
		   substExp default)
	      | RAISEexp (e,_) => substExp e
	      | IFexp {test, thenCase, elseCase} => 
                  (substExp test; substExp thenCase; substExp elseCase)
	      | ANDALSOexp (e1, e2) => (substExp e1; substExp e2)
	      | ORELSEexp (e1, e2) => (substExp e1; substExp e2)
	      | WHILEexp {test, expr} => (substExp test; substExp expr)
	      | LETexp (decl, body) => (substDec decl; substExp body)
	      | SEQexp exps => List.app substExp exps
	      | CONSTRAINTexp (e,_) => substExp e
	      | MARKexp (e,_) => substExp e
	      | _ => ()  (* datacons, constants, SELECTexp: nothing to do *)
	    (* end case *))
	and substDec dec =
            (case dec
	       of VALdec vbs => 
		    List.app (fn (VB{exp,...}) => substExp exp) vbs
	        | VALRECdec rvbs => 
		    List.app (fn (RVB{exp,...}) => substExp exp) rvbs
		| DOdec e => substExp e
		| LOCALdec (inner, outer) => (substDec inner; substDec outer)
		| SEQdec decs => List.app substDec decs
		| MARKdec (dec,_) => substDec dec
		| _ => ()  (* excluding type decls, abstype decls, module decls *)
	    (* end case *))
     in substExp exp
    end
*)

(* intToIntLiteral : int -> Types.ty IntConst.t *)
fun intToIntLiteral (n: int) =
    {ival = IntInf.fromInt n, ty = BasicTypes.intTy} : Types.ty IntConst.t

(* path compatibility *)

(* incompatible : path * path -> bool *)
(* Two paths are incompatible if they diverge at a choice (OR) node.
 * Paths that diverge at a product node (diff first at R links) are
 * compatible; paths that are prefix comparable are compatible. *)
fun incompatible (k1::rest1, k2::rest2) =
      if eqKey(k1,k2) then incompatible(rest1, rest2)
      else (case k1
	     of R _ => false
	      | _ => true)
  | incompatible (nil, path2) = false
  | incompatible (path1, nil) = false

(* consistentPath : path -> path -> bool *)
(* path1 (a var node path) is consistent with path2 (a decision point) *)
fun consistentPath path1 path2 = 
    case (path1, path2)
     of (R n1 :: rest1, R n2 :: rest2) =>
	if n1 = n2 then consistentPath rest1 rest2
        else true  (* consistent if diverge at R key *)
      | (k1 :: rest1, k2 :: rest2) =>
	if eqKey(k1,k2) then consistentPath rest1 rest2
	else false  (* branch at an OR node *)
      | (_, nil) => true (* consistent all choices on path2 *)
      | (nil, _) => true (* consistent down to variable node *)

(* uniqueVars : V.var list -> V.var list *)
(* elimiate redundant vars -- returns reduced list of vars with each var occurring
 * only once, in original order *)
fun uniqueVars vars =
    let fun unique (vs, nil) = rev vs
	  | unique (vs, v::others) =
	    if List.exists (fn var => V.eqVar(var, v)) vs
	    then unique (vs, others)     (* ignore v -- v already in vs *)
	    else unique (v::vs, others)  (* v is a new variable, add it *)
     in unique (nil, vars)
    end

(* infoToSvar : andor -> var *)
fun infoToSvar (info : AOinfo) =
    V.newVALvar (S.varSymbol (pathToString (#path info)), #typ info)

(* andorToSvar : andor -> var *)
fun andorToSvar andor =
    infoToSvar (getInfo andor)


(* alpha-converting svars in match code *)

(* FIX: not used 

datatype varStatus = NEW of svar | OLD of svar

exception TABLE

(* ASSUMPTION: once an svar is entered at its binding occurrence,
 * all applied occurrences will be refreshed before another
 * binding is encountered. *)

(* refresher : unit -> (svar -> svar) * (svar -> svar) *)
fun refresher () =
    let val vartable = LV.Tbl.create(100, TABLE) (* an lvar --> svar hashtable *)
	fun refreshBind svar =
	    let val lvar = V.varToLvar svar
		val svar' = OLD (LV.Tbl.lookup vartable lvar)
			    handle TABLE => NEW svar
	    in case svar'
		of NEW svar =>
		   (LV.Tbl.insert vartable (lvar,svar); svar)
		 | OLD svar =>
		    let val newlvar = LV.mkLvar()
			val newsvar = V.replaceLvar(svar,newlvar)
		    in LV.Tbl.insert vartable (lvar, newsvar);
		       newsvar
		    end
	    end
	fun refreshUse svar =
	    LV.Tbl.lookup vartable (varToLvar svar)
	    handle TABLE => bug "refreshOccurrence"
     in (refreshBind, refreshUse)
    end
*)
end (* local *)
end (* structure MCUtil *)
