(* FLINT/trans/protoandor.sml *)
(* revised "old" match compiler *)

(* build a simple "proto"-AndOr tree (type protoAndor), by layering the pattern infor for 
 * sequence of patterns (match left-hand-sides) to build up andorSimple nodes. *)

structure ProtoAndor =
struct

local
  structure DA = Access
  structure V = VarCon
  structure AS = Absyn
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure MC = MCCommon	     
  open Absyn MCCommon
  structure RS = RuleSet

  fun bug msg = ErrorMsg.impossible ("ProtoAndor: "^msg)

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

(* addVar : V.var * ruleno * simpleAndor -> simpleAndor *)
fun addVar (var: V.var, rule: ruleno, ANDs{bindings, children}) =
      ANDs {bindings=(var,rule)::bindings, children=children}
  | addVar (var, rule, ORs{bindings, sign, cases}) =
      ORs {bindings=(var,rule)::bindings, cases=cases, sign = sign}
  | addVar (var, rule, VARs{bindings}) =
      VARs {bindings=(var,rule)::bindings}

(* makeProtoAndor : pat list -> MC.protoAndor *)
fun makeProtoAndor (pats: AS.pat list) =
    (* ASSERT: not (null pats) *)

let (* genAndor : pat * ruleno -> andor *)
    fun genAndor (VARpat var, rule) = VARs {bindings = [(var, rule)]}
      | genAndor (WILDpat, _) = VARs {bindings = nil}
      | genAndor (CONSTRAINTpat(pat, _), rule) = genAndor(pat, rule) (* ignore type constraints *)
      | genAndor (LAYEREDpat(CONSTRAINTpat(lpat,_), bpat), rule) =
	  genAndor (LAYEREDpat(lpat, bpat), rule)
      | genAndor (LAYEREDpat(VARpat v, bpat), rule) =
	  addVar (v, rule, genAndor (bpat, rule))
      | genAndor (NUMpat(_, {ival, ty}), rule) =
	  let val con = numToCon(ival, ty)
	   in ORs {bindings = nil, sign = DA.CNIL, cases = [(con, RS.singleton rule, CONST)]}
	  end
      | genAndor (STRINGpat s, rule) =
	  ORs {bindings = nil, sign = DA.CNIL,	cases = [(STRINGcon s, RS.singleton rule, CONST)]}
      | genAndor (CHARpat s, rule) =
	  (* NOTE: this rule won't work for cross compiling to multi-byte characters. *) 
	  ORs{bindings = nil, sign = DA.CNIL, cases = [(charCon s, RS.singleton rule, CONST)]}
      | genAndor (RECORDpat{fields,...}, rule) =
	  ANDs{bindings = nil, children=multiGen(map #2 fields, rule)}
      | genAndor (VECTORpat(pats,ty), rule) =
	  ORs {bindings = nil, sign = DA.CNIL,
		cases = [(VLENcon (length pats, ty), RS.singleton rule,
			  VEC(multiGen(pats, rule)))]}
      | genAndor (CONpat(k,t), rule) =
	  ORs {bindings = nil, sign = TU.dataconSign k,
		cases = [(DATAcon(k, t), RS.singleton rule, CONST)]}
      | genAndor (APPpat(k,t,pat), rule) =
	  ORs {bindings = nil, sign = TU.dataconSign k,
		cases = [(DATAcon(k,t), RS.singleton rule, DCON(genAndor(pat, rule)))]}
      | genAndor _ =
	  bug "genandor - unexpected pat arg"

    (* multiGen : pat list * ruleno -> simpleAndor list *)
    and multiGen (pats, rule) = map (fn pat => genAndor(pat,rule)) pats

    (* mergeAndor : pat * andor * ruleno -> andor *)
    and mergeAndor (VARpat var, andor, rule) = addVar (var, rule, andor)
      | mergeAndor (WILDpat, andor, rule) = andor
      | mergeAndor (CONSTRAINTpat(pat, _), andor, rule) =
	  mergeAndor(pat, andor, rule)
      | mergeAndor (LAYEREDpat(CONSTRAINTpat(lpat,_), bpat), andor, rule) =
	  mergeAndor (LAYEREDpat(lpat, bpat), andor, rule)
      | mergeAndor (LAYEREDpat(VARpat var, bpat), andor, rule) =
	  addVar (var, rule, mergeAndor (bpat, andor, rule))
      | mergeAndor (CONpat(k,t), VARs{bindings}, rule) =
	  ORs {bindings = bindings, sign = TU.dataconSign k,
	       cases = [(DATAcon(k,t), RS.singleton rule, CONST)]}
      | mergeAndor (APPpat(k,t,pat), VARs{bindings}, rule) =
	  ORs {bindings = bindings, sign = TU.dataconSign k,
		cases = [(DATAcon(k,t), RS.singleton rule, DCON (genAndor(pat, rule)))]}
      | mergeAndor (pat, VARs{bindings}, rule) =
	  (case genAndor(pat, rule)
	     of ORs{bindings=nil, sign, cases} =>
		  ORs {bindings=bindings, sign=sign, cases=cases}
	      | ANDs{bindings=nil, children} =>
		  ANDs {bindings=bindings, children=children}
	      | VARs {bindings=newBindings} => 
		  VARs {bindings = newBindings @ bindings}
	      | _ => bug "mergeAndor - genAndor returned bogusly")
      | mergeAndor (NUMpat(_, {ival, ty}), c as ORs{bindings, cases, sign}, rule) =
	  let val con = numToCon(ival, ty)
	   in ORs{bindings = bindings, sign = sign,
		   cases = addACase(con, [], rule, cases)}
	  end
      | mergeAndor (NUMpat(_, {ival, ty}), c as ANDs _, rule) =
	  bug "mergeAndor - bad pattern merge: NUMpat ANDs"
      | mergeAndor (STRINGpat s, ORs{bindings, cases, sign}, rule) =
	  ORs {bindings = bindings, sign=sign,
		cases = addACase(STRINGcon s, nil, rule, cases)}

      (* NOTE: the following won't work for cross compiling
       * to multi-byte characters *)
      | mergeAndor (CHARpat s, ORs{bindings, cases, sign}, rule) =
	  ORs{bindings = bindings, sign=sign,
	       cases = addACase(charCon s, nil, rule, cases)}
      | mergeAndor (RECORDpat{fields,...},
		    ANDs{bindings, children}, rule) =
	  ANDs{bindings = bindings,
	      children=multiMerge(map #2 fields, children, rule)}
      | mergeAndor (VECTORpat(pats,t), ORs{bindings, cases, sign}, rule) =
	  ORs {bindings = bindings, sign = sign,
		cases = addACase(VLENcon(length pats, t),pats,rule,cases)}
      | mergeAndor (CONpat(k,t), ORs{bindings, cases, sign}, rule) =
	  ORs {bindings=bindings, sign=sign,
		cases=addACase(DATAcon(k,t), nil, rule, cases)}
      | mergeAndor (APPpat(k,t,pat), ORs{bindings, cases, sign}, rule) =
	  ORs {bindings=bindings, sign=sign,
		cases=addACase(DATAcon(k,t), [pat], rule, cases)}
      | mergeAndor _ =
	  bug "mergeAndor - unexpected arg"

    (* addACase : con * pat list * ruleno * simpleVariant list -> simpleVariant list *)
    (* if con is constant, pats is nil;
     * if con is a normal nonconstant datacon, pats is a singleton;
     * if con is VLENcon(k,_), pats has length = k (the vector length) *)  
    and addACase (con, pats, rule, nil) =  (* a new con has been discovered *)
	  let val subcase =
		  case (con, pats)
		   of ((INTcon _ | WORDcon _ | STRINGcon _ | DATAcon _), nil) => CONST
		    | (DATAcon _, [pat]) => DCON (genAndor (pat, rule))
		    | (VLENcon _, pats) => VEC (multiGen(pats, rule))
		    | _ => bug "addACase new"
	   in [(con, RS.singleton rule, subcase)]
	  end
      | addACase (con, pats, rule, (aCase as (con', rules, subcase)) :: rest) =
	  if conEq (con, con')  (* existing variant with same con *)
	  then let val subcase' =
		       case (subcase, pats)
			of (CONST, nil) => CONST
			 | (DCON sandor, [pat]) =>
			     DCON (mergeAndor (pat, sandor, rule))
			 | (VEC velems, _) => VEC (multiMerge(pats, velems, rule))
			 | _ => bug "addACase old"
	        in (con, RS.add(rules, rule), subcase') :: rest
	       end
	  else aCase::(addACase(con, pats, rule, rest))

    (* multiMerge : pat list * simpleAndor list * int -> andor list *)
    and multiMerge (pats, sandors, rule) =
	let fun merge1 (pat, sandor) = mergeAndor (pat, sandor, rule)
	 in ListPair.mapEq merge1 (pats, sandors)
	end

    (* mergePats : pat list * int -> simpleAndor *)
    fun mergePats (nil, _, sandor) = sandor
      | mergePats (pat::rest, rule, sandor) =
	  mergePats (rest, rule+1, mergeAndor (pat, sandor, rule))

    val pat0 :: rest = pats

 in mergePats (rest, 1, genAndor (pat0, 0))
end (* fun makeProtoAndor *)

end (* top local *)

end (* structure ProtoAndor *)
