(* andor.sml *)

(* build "proto"-AndOr tree (type andor0) *)

structure Andor0 =
struct

local
  structure DA = Access
  structure AS = Absyn
  structure TU = TypesUtil
  structure BT = BasicTypes
  open Absyn MCCommon
  structure RS = RuleSet
in

fun numToCon (v, ty) =
    let fun mkWORD sz = WORDcon{ival = v, ty = sz}
	fun mkINT sz = INTcon{ival = v, ty = sz}
     in if TU.equalType(ty, BT.intTy)
	  then mkINT Target.defaultIntSz
	else if TU.equalType(ty, BT.int32Ty)
	  then mkINT 32
	else if TU.equalType(ty, BT.int64Ty)
	  then mkINT 64
	else if TU.equalType(ty, BT.intinfTy)
	  then mkINT 0
	else if TU.equalType(ty, BT.wordTy)
	  then mkWORD Target.defaultIntSz
	else if TU.equalType(ty, BT.word8Ty)
	  then mkWORD Target.defaultIntSz
	else if TU.equalType(ty, BT.word32Ty)
	  then mkWORD 32
        else if TU.equalType(ty, BT.word64Ty)
          then mkWORD 64
	  else bug "numToCon: unrecognized numeric type"
      end

(* default integer pattern constant *)
fun intCon n = INTcon {ival = IntInf.fromInt n, ty = Target.defaultIntSz}

(* pattern constant for character literal *)
(* QUESTION: perhaps this should be a Word8.word literal? *)
fun charCon s = intCon (Char.ord (String.sub (s, 0)))


(* makeAndor : pat list -> MCCommon.andor0 *)
fun makeAndor (pats: AS.pat list) =
    (* ASSERT: not (null pats) *)
let (* addBinding : var * ruleno * andor -> andor *)
    fun addBinding (v, rule, AND0{bindings, children}) =
	  AND0 {bindings=(rule,v)::bindings, children=children}
      | addBinding (v, rule, OR0{bindings, sign, cases}) =
	  OR0 {bindings=(rule,v)::bindings, cases=cases, sign = sign}
      | addBinding (v, rule, LEAF0{bindings}) =
	  LEAF0 {bindings=(rule,v)::bindings}

    (* genAndor : pat * ruleno -> andor *)
    and genAndor (VARpat v, rule) = LEAF0 {bindings = [(rule, v)]}
      | genAndor (WILDpat, _) = LEAF0 {bindings = nil}
      | genAndor (CONSTRAINTpat(pat, _), rule) = genAndor(pat, rule) (* ignore type constraints *)
      | genAndor (LAYEREDpat(CONSTRAINTpat(lpat,_), bpat), rule) =
	  genAndor (LAYEREDpat(lpat, bpat), rule)
      | genAndor (LAYEREDpat(VARpat v, bpat), rule) =
	  addBinding (v, rule, genAndor (bpat, rule))
      | genAndor (NUMpat(_, {ival, ty}), rule) =
	  let val con = numToCon(ival, ty)
	   in OR0 {bindings = nil, sign = DA.CNIL, cases = [(con, RS.singleton rule, CONST)]}
	  end
      | genAndor (STRINGpat s, rule) =
	  OR0 {bindings = nil, sign = DA.CNIL,	cases = [(STRINGcon s, RS.singleton rule, CONST)]}

	(* NOTE: the following won't work for cross compiling
	 *       to multi-byte characters. *)

      | genAndor (CHARpat s, rule) =
	  OR0{bindings = nil, sign = DA.CNIL, cases = [(charCon s, RS.singleton rule, CONST)]}
      | genAndor (RECORDpat{fields,...}, rule) =
	  AND0{bindings = nil, children=multiGen(map #2 fields, rule)}
      | genAndor (VECTORpat(pats,t), rule) =
	  OR0 {bindings = nil, sign = DA.CNIL,
		cases = [(VLENcon (length pats, t), RS.singleton rule,
			  VEC(multiGen(pats, rule)))]}
      | genAndor (CONpat(k,t), rule) =
	  OR0 {bindings = nil, sign = TU.dataconSign k,
		cases = [(DATAcon(k, t), RS.singleton rule, CONST)]}
      | genAndor (APPpat(k,t,pat), rule) =
	  OR0 {bindings = nil, sign = TU.dataconSign k,
		cases = [(DATAcon(k,t), RS.singleton rule, DCON(genAndor(pat, rule)))]}
      | genAndor _ =
	  bug "genandor - unexpected pat arg"

    (* multiGen : pat list * ruleno -> andor list *)
    and multiGen (pats, rule) = map (fn pat => genAndor(pat,rule)) pats

    (* mergeAndor : pat * andor * ruleno -> andor *)
    and mergeAndor (VARpat v, andor, rule) = addBinding (v, rule, andor)
      | mergeAndor (WILDpat, andor, rule) = andor
      | mergeAndor (CONSTRAINTpat(pat, _), andor, rule) =
	  mergeAndor(pat, andor, rule)
      | mergeAndor (LAYEREDpat(CONSTRAINTpat(lpat,_), bpat), andor, rule) =
	  mergeAndor (LAYEREDpat(lpat, bpat), andor, rule)
      | mergeAndor (LAYEREDpat(VARpat v, bpat), andor, rule) =
	  addBinding (v, rule, mergeAndor (bpat, andor, rule))
      | mergeAndor (CONpat(k,t), LEAF0{bindings}, rule) =
	  OR0 {bindings = nil, sign = TU.dataconSign k,
		cases = [(DATAcon(k,t), RS.singleton rule, CONST)]}
      | mergeAndor (APPpat(k,t,pat), LEAF0{bindings}, rule) =
	  OR0 {bindings = bindings, sign = TU.dataconSign k,
		cases = [(DATAcon(k,t), RS.singleton rule, DCON (genAndor(pat, rule)))]}
      | mergeAndor (pat, LEAF0{bindings}, rule) =
	  (case genAndor(pat, rule)
	     of OR0{bindings=nil, sign, cases} =>
		  OR0{bindings=bindings, sign=sign, cases=cases}
	      | AND0{bindings=nil, children} =>
		  AND0{bindings=bindings, children=children}
	      | _ => bug "mergeAndor - genAndor returned bogusly")
      | mergeAndor (NUMpat(_, {ival, ty}), c as OR0{bindings, cases, sign}, rule) =
	  let val con = numToCon(ival, ty)
	   in OR0{bindings = bindings, sign = sign,
		   cases = addACase(con, [], rule, cases)}
	  end
      | mergeAndor (NUMpat(_, {ival, ty}), c as AND0 _, rule) =
	  bug "mergeAndor - bad pattern merge: NUMpat AND0"
      | mergeAndor (STRINGpat s, OR0{bindings, cases, sign}, rule) =
	  OR0 {bindings = bindings, sign=sign,
		cases = addACase(STRINGcon s, nil, rule, cases)}

      (* NOTE: the following won't work for cross compiling
       * to multi-byte characters *)
      | mergeAndor (CHARpat s, OR0{bindings, cases, sign}, rule) =
	  OR0{bindings = bindings, sign=sign,
	       cases = addACase(charCon s, nil, rule, cases)}
      | mergeAndor (RECORDpat{fields,...},
		    AND0{bindings, children}, rule) =
	  AND0{bindings = bindings,
	      children=multiMerge(map #2 fields, children, rule)}
      | mergeAndor (VECTORpat(pats,t), OR0{bindings, cases, sign}, rule) =
	  OR0 {bindings = bindings, sign = sign,
		cases = addACase(VLENcon(length pats, t),pats,rule,cases)}
      | mergeAndor (CONpat(k,t), OR0{bindings, cases, sign}, rule) =
	  OR0 {bindings=bindings, sign=sign,
		cases=addACase(DATAcon(k,t), nil, rule, cases)}
      | mergeAndor (APPpat(k,t,pat), OR0{bindings, cases, sign}, rule) =
	  OR0 {bindings=bindings, sign=sign,
		cases=addACase(DATAcon(k,t), [pat], rule, cases)}
      | mergeAndor (CONpat _, AND0 _, _) =
	  bug "mergeAndor - merging constant constructor into record"
      | mergeAndor (APPpat _, AND0 _, _) =
	  bug "mergeAndor - merging applied constructor into record"
      | mergeAndor _ =
	  bug "mergeAndor - unexpected arg"

    (* addACase : con * pat list * ruleno * andorCase list -> andorCase list *)
    (* if con is constant, pats is nil;
     * if con is a normal nonconstant datacon, pats is a singleton;
     * if con is VLENcon(k,_), pats has length = k (the vector length) *)  
    and addACase (con, pats, rule, nil) =
	  let val subcase =
		  case (conKind con, pats)
		   of (CONSTk, nil) => CONST
		    | (DCONk, [pat]) => DCON (genAndor (pat, rule))
		    | (VLENk, _) => VEC (multiGen(pats, rule))
		    | _ => bug "addACase new"
	   in [(con, RS.singleton rule, subcase)]
	  end
      | addACase (con, pats, rule, (aCase as (con', rules, subcase)) :: rest) =
	  if conEq (con, con')
	  then let val subcase' =
		       case (subcase, pats)
			of (CONST, nil) => CONST
			 | (DCON andor, [pat]) =>
			     DCON (mergeAndor (pat, andor, rule))
			 | (VEC velems, _) => VEC (multiMerge(pats, velems, rule))
			 | _ => bug "addACase old"
	        in (con, RS.add(rules, rule), subcase') :: rest
	       end
	  else aCase::(addACase(con, pats, rule, rest))

    (* multiMerge : pat list * (andor list) list * int -> andor list *)
    and multiMerge (nil, nil, rule) = nil
      | multiMerge (pat::pats, child::children, rule) =
	 (mergeAndor(pat, child, rule))::(multiMerge(pats, children, rule))
      | multiMerge _ = bug "multiMerge - list length mismatch"

    (* makeAndor' : pat list * int -> (path * andor) list *)
    fun makeAndor' ([pat], rule) = genAndor(pat, rule)
      | makeAndor' (pat::rest, rule) =
	  mergeAndor (pat, makeAndor'(rest, rule+1), rule)
      | makeAndor' (nil, _) = bug "makeAndor' - no rules"

 in makeAndor' (pats, 0)
end (* fun makeAndor *)

end (* local *)

end (* structure Andor0 *)
