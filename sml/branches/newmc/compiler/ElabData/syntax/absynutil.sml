(* absynutil.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * More stuff from ElabUtil should be moved here eventually.
 *)
structure AbsynUtil :
sig
  val bogusCON : Types.datacon
  val bogusEXN : Types.datacon
  val unitExp : Absyn.exp
  val unitPat : Absyn.pat
  val isWild : Absyn.pat -> bool
  val TUPLEexp : Absyn.exp list -> Absyn.exp
  val TUPLEpat : Absyn.pat list -> Absyn.pat
  val stripPatMarks : Absyn.pat -> Absyn.pat
  val patternVars : Absyn.pat -> VarCon.var list

end =

struct

local
  structure S = Symbol
  structure T = Types
  structure LV = LambdaVar
  structure A = Access
  structure V = VarCon
  open Absyn
in

  (* "special" datacons -- moved from VarCon (ElabData/syntax/varcon.s??) *)

  val bogusCON =
      T.DATACON{name=S.varSymbol "bogus",
		typ=T.WILDCARDty,
		rep=A.CONSTANT 0,
		const=true,
		lazyp=false,
		sign=A.CSIG(0,1)}

  val bogusEXN =
      T.DATACON{name=S.varSymbol "bogus",
		typ=BasicTypes.exnTy,
		rep=A.CONSTANT 0,
		const=true,
		lazyp=false,
		sign=A.CNIL}

  val unitExp = RECORDexp []
  val unitPat = RECORDpat {fields=nil, flex=false, typ=ref(BasicTypes.unitTy)}

  (* isWild : pat -> bool *)
  fun isWild WILDpat = true
    | isWild _ = false

  fun TUPLEexp l =
      let fun build (_, []) = []
	    | build (i, e :: es) =
	      (LABEL { number = i-1, name = Tuples.numlabel i }, e)
	      :: build (i+1, es)
       in RECORDexp (build (1, l))
      end

  fun TUPLEpat l =
      let fun build (_, []) = []
	    | build (i, e :: es) = (Tuples.numlabel i, e) :: build (i+1, es)
       in RECORDpat { fields = build (1, l), flex = false,
		      typ = ref Types.UNDEFty }
      end

fun stripPatMarks pat =
    case pat
      of (MARKpat(p,_)) => stripPatMarks p
       | RECORDpat{fields, flex, typ} =>
	 RECORDpat{fields = map (fn (l,p) => (l, stripPatMarks p)) fields,
		   flex = flex, typ = typ}
       | APPpat (dc, tvs, p) =>
         APPpat (dc, tvs, stripPatMarks p)
       | CONSTRAINTpat (p, ty) => CONSTRAINTpat (stripPatMarks p, ty)
       | LAYEREDpat(pat1, pat2) =>
	 LAYEREDpat(stripPatMarks pat1, stripPatMarks pat2)
       | ORpat (pat1, pat2) =>
         ORpat (stripPatMarks pat1, stripPatMarks pat2)
       | VECTORpat (pats,ty) =>
	 VECTORpat (map stripPatMarks pats, ty)
       | p => p

(* patternVars: Absyn.pat -> VarCon.var list *)
(* returns a list of the variables bound in the pattern, without duplicates, and
 * sorted in order of their lvars (all variables in a pattern have access LVAR).
 * WILDpat is not considered a pattern variable.  *)
fun patternVars pat =
    let fun enter (new : V.var, l: V.var list) =
	    let fun f [] = [new]
		  | f (l as h::t) =
		    case LV.compare (V.varToLvar new, V.varToLvar h)
		     of LESS    => new::l
		      | GREATER => h::f t
		      | EQUAL   => l
	     in f l
	    end
	fun collect(pat::rest,vars) =  
	    (case pat
	      of VARpat var => collect(rest, enter(var,vars))
	       | RECORDpat{fields, ...} => collect (map #2 fields @ rest, vars)
	       | APPpat(_,_,p) => collect(p::rest, vars)
	       | CONSTRAINTpat (p,_) => collect(p::rest,vars)
	       | LAYEREDpat(pat1, pat2) => collect(pat1::pat2::rest, vars)
	       | ORpat(pat1, pat2) => collect(pat1::rest, vars)  (* pat1 and pat2 have same variables *)
	       | VECTORpat(pats,_) => collect(pats@rest, vars)
	       | _ => collect(rest, vars))
	  | collect (nil, vars) = vars
    in collect ([pat],nil)
    end

end (* local *)
end (* structure AbsynUtil *)
