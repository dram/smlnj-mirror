(* vmcexp.sml *)

(* A "virtual" version of mcexps where pseudo-constructors produce absyn expressions.
 * A pseudo-constructor function is defined for each constructor of the (former) mcexp
 * datatype (mcexp.sml).  E.g. function Var for mcexp constructor Var. *)

structure VMCexp =
struct

local
  structure S = Symbol
  structure AS = Absyn
  structure AU = AbsynUtil
  structure T = Types
  structure BT = BasicTypes
  structure SV = SVar  (* simplified (virtual) variables *)
  structure V = VarCon
  structure MT = MCTypes
  structure MU = MCUtil
  open Absyn

  fun bug (msg: string) = ErrorMsg.impossible ("VMCexp: "^msg)
in

type mcexp = AS.exp

type caseVariant = MT.key * SV.svar option * mcexp

(* mkVarExp : SV.svar -> AS.exp *)
fun mkVarExp svar =
    VARexp(ref(SV.svarToVar svar), nil)

(* mkNumExp : int -> AS.exp *)
fun mkNumExp (n: int) =
    NUMexp(Int.toString n, MU.intToIntLiteral n)

(* Var: SV.svar -> mcexp *)
(* Convert an svar to a VarCon.var and treat as expression.
 * These variables are "lambda-bound", so not polymorphic,
 * hence no instantiation types. *)
fun Var (sv : SV.svar) = AS.VARexp(ref(SV.svarToVar sv), nil)

(* mkLetVar : V.var * mcexp * mcexp -> mcexp *)
(* "let var = defexp in body" *)
fun mkLetVar (var: V.var, defexp, body) =
    LETexp(VALdec [VB{pat = VARpat var,
		      exp = defexp,
		      typ = V.varType var,
		      boundtvs = nil,
		      tyvars = ref nil}],
	   body)

(* mkLetSvar : SV.svar * mcexp * mcexp -> mcexp *)
(* "let svar = defexp in body" *)
fun mkLetSvar (svar: SV.svar, defexp, body) =
    mkLetVar(SV.svarToVar svar, defexp, body)

(* keyToPat : MT.key * SV.svar option -> AS.pat *)
(* svarOp is SOME sv if key is D dcon where dcon is not constant *)
fun keyToPat (key, svarOp) =
    (case key
      of (MT.D (dcon,tvs)) =>
	   (case svarOp
	     of NONE => CONpat(dcon, tvs)  (* => dcon is constant *)
	      | SOME sv => APPpat(dcon, tvs, VARpat(SV.svarToVar sv)))
       | MT.I num => NUMpat ("", num)
       | MT.W num => NUMpat ("", num)
       | MT.S s => STRINGpat s
       | MT.C c => CHARpat (Char.toString c)
       | _ => bug "keyToPat") (* does not apply to V, R keys *)

(* Letr : SV.svar list * SV.svar * mcexp -> mcexp *)
(* "let (sv1,...,svn) = sv0 in body"; destructure a record/tuple *)
fun Letr (svars, svar, body) =
    let val defvar = SV.svarToVar svar  (* variable bound to the record/tuple *)
	fun wrapLets (nil, _) = body
	  | wrapLets (sv::rest, n) =
	      mkLetSvar (sv, AS.SELECTexp(defvar, n, true), (* record selection *)
		    wrapLets (rest, n+1))
    in wrapLets (svars, 0)  (* selection index 0 based *)
    end

(* Letv : SV.svar list * SV.svar * mcexp -> mcexp *)
(* "let #[sv1,...,svn] = sv0 in body"; destructure a vector *)
(* Vector selection represented by SELECTexp with third element "false". *)
fun Letv (svars, svar, body) =
    let val defvar = SV.svarToVar svar
	fun wrapLets (nil, _) = body
	  | wrapLets (sv::rest, n) =
	      mkLetSvar (sv, AS.SELECTexp(defvar, n, false), (* vector selection *)
		    wrapLets (rest, n+1))
    in wrapLets (svars, 0)
    end

(* Letf : SV.svar * mcexp * mcexp -> mcexp *)
(* "let svar = funexp in body" *)
fun Letf (svar, funexp, body) = mkLetSvar (svar, funexp, body)

(* Case : SV.svar * caseVariant list * mcexp option -> mcexp *)
(* The default is now incorporated into the SWITCHexp. It will always be SOME
 * if the patterns in rules are not exhaustive. *)
fun Switch (svar: SV.svar, cases, defaultOp) =
    (case cases  (* distinguish vector switch special case by key = V _ *)
       of nil => bug "Switch: empty cases"
        | ((MT.V _, _, _) :: _) => (* vector length switch *)
	    (* "let val len = Vector.length svar in <<switch over int values of len>>"
             * where "len" is a fresh internal variable -- this is generated in 
             * the VSWITCHexp case of Translate.mkExp0 to avoid the problem of
	     * accessing the vector length primop in absyn. *)
	    let fun docase (MT.V n, _, rhsexp) =
		      RULE (NUMpat("", MU.intToIntLiteral n), rhsexp)
		  | docase _ = bug "Switch:vector case: key not V"
	     in AS.VSWITCHexp(SV.svarToVar svar, map docase cases, Option.valOf defaultOp)
	    end
	|  _ =>  (* the general case, dispatching on the key *)
	    let fun docase (key, svarOp, rhsexp) = RULE(keyToPat(key,svarOp), rhsexp)
	     in AS.SWITCHexp(SV.svarToVar svar, map docase cases, defaultOp)
	    end)

(* Sfun : V.var list * mcexp -> mcexp *)
(* "fn (v0, ..., vn) => rhsExp"; functionalized, multi-use rule RHS
    Note that the body exp is from the original rule, and hence will only
    contain occurrences of pattern vars, never match svars. *)
(* FIXED: Translate.mkExp0 expects single rule of form (VARpat v, body).
 * need to pass single parameter and destruct the tuple around body.
 * BUG? pvars for different functions may overlap, introducing duplication
 * among lambda bound lvars. Do lvar alpha-conversion to make bound lvars
 * unique? (fcontract problem). *)
fun Sfun (pvars, body) =
    let val pvarsTy = BasicTypes.tupleTy(map V.varType pvars)
	val argvar = V.newVALvar (S.varSymbol "SfunArg", pvarsTy)
	fun wrapLets (nil, _) = body
	  | wrapLets (v::rest, n) =
	      mkLetVar (v, AS.SELECTexp(argvar, n, true), (* record selection *)
			 wrapLets (rest, n+1))
	val wrappedBody = wrapLets (pvars, 0)
	val rule = RULE(AS.VARpat argvar, wrappedBody)
     in AS.FNexp([rule], pvarsTy, T.UNDEFty) (* result not relevant *)
    end

(* Sapp : SV.svar * SV.svar list -> mcexp *)
(* passing a single tuple value, consistent with Translate. *)
fun Sapp (funsvar, argsvars) =
    AS.APPexp(Var funsvar, AU.TUPLEexp(map Var argsvars))

(* Failure : datacon * ty -> mcexp *)
(* "raise matchExn"; causes appropriate exception (Match or Bind) to be
 * raised if match fails, with the "result" type resTy. *)
fun Failure (matchExn, resTy) = RAISEexp(CONexp(matchExn, nil), resTy)
(* FIX: T.UNDEFty won't do. The type arg of RAISEexp defined by the
 * type checker is a metatype variable that is unified with the context type.
 * In this case, that context type should be the "result" type of the match,
 * which is passed as the resTy parameter. *)

end (* local *)
end (* structure Vcode *)
