(* andor.sml *)

structure AndOr =
struct

local
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure IC = IntConst
  open Absyn MCCommon
  (* also used: IntInf, Target, Char, String *)
in

(* test for 64-bit int/word types, which are represented as pairs of 32-bit words *)
(* not used!?
fun isInt64 ty = TU.equalType(ty, BT.int64Ty)
fun isWord64 ty = TU.equalType(ty, BT.word64Ty)
*)

fun numCon (v, ty, msg) =
    let fun mkWORD sz = IC.WORDconst {ival = v, ty = sz}
	fun mkINT sz = IC.INTconst {ival = v, ty = sz}
    in
	if TU.equalType(ty, BT.intTy)
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
(* QUESTION: perhaps we should preserve the size (e.g., in the case of
 * word8) for better jump tables? *)
	  then mkWORD Target.defaultIntSz
	else if TU.equalType(ty, BT.word32Ty)
	  then mkWORD 32
        else if TU.equalType(ty, BT.word64Ty)
          then mkWORD 64
	  else bug msg
    end

(* default integer pattern constant *)
fun intCon n = IC.INTconst{ival = IntInf.fromInt n, ty = Target.defaultIntSz}

(* pattern constant for character literal *)
(* QUESTION: perhaps this should be a Word8.word literal? *)
fun charCon s = intCon (Char.ord (String.sub (s, 0)))

(* addAsBinding : binding * andor -> andor *)
fun addAsBinding (b, AS(andor, bindings)) = AS(andor, b::bindings)
  | addAsBinding (b, andor) = AS(andor, [b])

(* addVarBinding : binding * andor -> andor *)
fun addVarBinding (b, VARS(andor, bindings)) = VARS(andor, b::bindings)
  | addVarBinding (b, andor) = VARS(andor, [b])

(* merge AS bindings so there is only one layer of AS for an andor tree *)
fun mergeAsBindings (bindings, AS(andor, bindings')) =
      (* could "merge" the two bindings lists instead of just appending them *)
      AS(andor, bindings@bindings')
  | mergeAsBindings (bindings, andor) = AS(andor, bindings)

(* addConstChild: constCon * ruleno * constChild list -> constChild list *)
fun addConstChild (const, rule, nil) =
      [(const, R.singleton rule)]
  | addConstChild (const, rule, (branch as (const', rules))::rest) =
      if constantEq(const, const')
      then (const, R.add(rules,rule))::rest
      else branch::(addConstChild(const, rule, rest))

(* makeAndor : pat list -> andor *)
(* looks like makeAndor only depends on the first (path,pat) component of each triple
 * forming an element of the matchRep argument (which is a list of triples) *)
fun makeAndor pats =
let
    (* initAnd : pat list * ruleno -> andor list
     * preserves order of pattern list *)
    fun initAnd (nil, rule) = nil
      | initAnd (pat::rest, rule) = (initAndor(pat,rule))::initAnd((rest,rule))

    (* initAndor : pat * ruleno -> andor *)
    and initAndor (VARpat var, rule) =
	  VARS (VAR, [(var,rule)])
      | initAndor (WILDpat, rule) =
          (* wildcard pat treated as a special variable pattern *)
	  VARS (VAR, [(wildVar, rule)])
      | initAndor (LAYEREDpat(var,_,basepat), rule) = (* ignoring constraint option *)
	  addAsBinding ((var,rule), initAndor (basepat, rule))
      | initAndor (NUMpat(_, {ival, ty}), rule) =
	  ORconst [(numCon(ival, ty, "initAndor NUMpat"), [rule])]
      | initAndor (STRINGpat s, rule) =
	  ORconst [(STRINGconst s, [rule])]
      | initAndor (CHARpat s, rule) =
	  (* NOTE: this won't work for cross compiling to multi-byte characters. *)
	  ORconst [(charCon s, [rule])]
      | initAndor (RECORDpat{fields,...}, rule) =
	  AND (initAnd(map #2 fields, rule)}
      | initAndor (VECTORpat(pats,ty), rule) =
	  ORvec (ty, [((length pats), AND(initAnd(pats, rule)), R.singleton rule)]) 
      | initAndor (CONpat(dcon,tvs), rule) =
	  ORdata [((dcon, tvs), NONE, [rule])]
      | initAndor (APPpat(k,tvs,pat), rule) =
	  ORdata [((k,tvs), SOME(AND(initAndor(pat, rule))), R.singleton rule)]
      | initAndor _ =
	  bug "initAndor - unexpected pat arg"

    (* DBM: what are the invariants of merging?  E.g.
        - Should WILDCARD and variables "absorb" any futher patterns? NO! Example 2.
        - If there is a dominating discrimination, will two nodes ever be merged? NO.
        - There are no nested BIND or DEFAULT constructors.  They always merge into one.
        - Patterns that are merged into an andor tree have the same type, and that
          applies to subpatterns all the way down to the leaves. This implies that
          a record pattern will only be merged with a "record" (ALL) andor node,
          never with a ORdata or ORconst or ORvec andor node (which are varieties of OR nodes).
        - vector element patterns will only be merged with andor nodes (lists) of the
          same length (after discriminating on the length at a ORvec node).
     *)

    (* mergeAndor : pat * andor * ruleno -> andor *)
    and mergeAndor (VARpat v, andor, rule) =
	  addVarBinding ((v,rule), andor)
      | mergeAndor (WILDpat, andor, rule) =
	  addVarBinding ((wildVar,rule), andor)
      | mergeAndor (CONSTRAINTpat(pat, _), andor, rule) =
	  (* disregard constraint *)
	  mergeAndor(pat, andor, rule)
      | mergeAndor (LAYEREDpat(v,_,basepat), andor, rule) =
	  addAsBinding ((v,rule), mergeAndor (basepat, andor, rule))
      | mergeAndor (NUMpat(_, {ival, ty}), ORconst constChildren, rule) =
	  let val const = numCon(ival, ty, "mergeAndor NUMpat")
	   in ORconst (addConstChild(const, rule, constChildren))
	  end
      | mergeAndor (STRINGpat s, ORconst constChildren, rule) =
	  ORconst(addConstChild(STRINGconst s, rule, constChildren))
      | mergeAndor (CHARpat s, ORconst constChildren, rule) =
          (* NOTE: this won't work for cross compiling to multi-byte characters *)
          ORconst (addConstChild(charCon s, rule, constChildren))
      | mergeAndor (RECORDpat{fields,...}, AND elements, rule) =
	  (* arity of record and AND andor node are equal *)
	  AND (mergeAnd (map #2 fields, elements, rule))
      | mergeAndor (VECTORpat(pats,ty), ORvec(ty', vecChildren), rule) =
	  (* assume ty and ty' are equal *)
	  ORvec(mergeVector (pats,rule,vecChildren))
      | mergeAndor (CONpat(dcon,tvs), ORdata dataChildren, rule) =
	  ORdata (mergeData (dcon, NONE, dataChildren, rule))
      | mergeAndor (APPpat(dcon,tvs,pat), ORdata dataChildren, rule) =
	  ORdata (mergeData (dcon, SOME pat, dataChildren, rule))
      | mergeAndor (pat, VAR, rule) =
	  initAndor (pat, rule)
      | mergeAndor (pat, AS(andor,bindings), rule) =
	  mergeAsBindings (mergeAndor(pat,andor,rule), bindings)
      | mergeAndor (pat, VARS(andor, defaults), rule) =
      (* remaining cases impossible: incompatible types *)
      | mergeAndor (NUMpat _, AND _, _) =
          bug "mergeAndor: NUMpat - AND"
      | mergeAndor (CONpat _, AND _, _) =
	  bug "mergeAndor: CONpat - AND"
      | mergeAndor (APPpat _, AND _, _) =
	  bug "mergeAndor: APPpat - AND"
      | mergeAndor _ =
	  bug "mergeAndor: incompatible pat and andor tree"

    and mergeAnd (nil, nil, rule) = nil
      | mergeAnd (pat::pats, elem::elems, rule) =
	 (mergeAndor(pat, elem, rule))::(mergeAnd(pats, elems, rule))
      | mergeAnd _ = bug "mergeAnd - list length mismatch" (* impossible *)

    and mergeVector (pats,rule,vecChildren: vecChild list) : vecChild list =
	let val len = length pats (* could be 0 *)
	    fun merge (vbs as (vb as (len', AND andors, ruleset)::rest), others) =
		if len < len'  (* new vector length *)
		then List.revAppend(others,
		       (len, AND(initAnd pats), R.singleton rule)::vbs)
		else if len = len'
		then List.revAppend(others,
		       (len, mergeAnd(pats, AND andors), R.add(ruleset,rule))::rest)
		else merge (rest, vb::others)
	      | merge (nil,others) = rev((len, AND(initAnd pats), R.singleton rule)::others)
	 in merge(vecChildren,nil)
	end

    and mergeData (dcon, patOp, dataChildren, rule) =
	let fun merge ((dcon',andorOp,ruleset)::rest, others) =
		  if eqDcon(dcon,dcon')
		  then
		    let val dataChild =
			    case (patOp, andorOp)
			      of (NONE, NONE) =>  (* constant dcon *)
				  (dcon, NONE, R.add(ruleset,rule))
			       | (SOME pat, SOME andor) => 
				  (dcon, SOME(mergeAndor(pat,andor,rule)),
				   R.add(ruleset,rule))
			       | _ => bug "mergeData 1"
		      in List.revAppend (others, dataChild::rest)
		     end
		  else merge(rest, db::others)
	      | merge (nil, others) =  (* dcon is new *)
		  let val dataChild =
			  case patOp
			    of NONE =>
				(dcon, NONE, R.singleton rule)
			     | SOME pat =>
			        (dcon, SOME(initAndor(pat,rule)), R.singleton rule)
		   in rev (dataChild::others)
		  end
	      | merge _ = bug "mergeData 2"
	 in merge(dataChildren`, nil)
	end

    (* makeAndor' : pat list * ruleno -> andor *)
    fun makeAndor' (nil, rule) = bug "makeAndor' - no rules"
      | makeAndor' ([pat], rule) = initAndor (pat, rule)
      | makeAndor' (pat::pats, rule) =   (* at least 2 pats, |rest| >= 1 *)
	  mergeAndor(pat, makeAndor'(pats, next(rule)), rule)

in makeAndor' (pats,0)

end (* fun makeAndor *)

end (* local open *)

end (* structure AndOr *)
