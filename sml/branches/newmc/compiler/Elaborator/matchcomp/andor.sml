(* FLINT/trans/andor.sml *)
(* revised "old" match compiler *)

(* Translate proto-andor trees (andorSimple) to andor trees. This involves
 *  (1) computing the "defaults" rule set for OR nodes, 
 *  (2) converting the "bindings" field of andor0 (varBindings) to bindings as ruleset
 *      (extracting the rules from bindings and discarding the variables, but "filtering"
 *      with the inherited "active" ruleset).
 *  (3) adding an id field with an unique nodeId number
 *  (This was originally derived from the "flatten" functions (e.g. "flatteningAndor) of mc99.)
 *)

structure Andor =
struct

local
  structure LV = LambdaVar
  structure M = LV.Map
  structure V = VarCon
  structure PP = PrettyPrint
  open MCCommon
  structure RS = RuleSet

  val debugging = MCControl.mcdebugging

  fun bug msg = ErrorMsg.impossible ("Andor: " ^ msg)

  val say = Control_Print.say
  fun newline () = say "\n"
  fun saynl msg = (say msg; newline())
  fun says strings = saynl (concat strings)

  fun dbsay msg =
      if !debugging
      then (say msg; newline())
      else ()
  fun dbsays msgs = dbsay (concat msgs)

  fun ppCon con =
      PP.with_default_pp (fn ppstrm => MCPrint.ppCon ppstrm con)

  fun ppRules (rules: ruleset) =
      PP.with_default_pp (fn ppstrm => MCPrint.ppRuleset ppstrm rules)

in

(* node ids (: int) *)
val idcount = ref 0;
fun newId () = !idcount before (idcount := !idcount + 1)
fun resetId () = (idcount := 0)

(* pvarmap construction *)

(* a pvarmap maps lvars (of pattern variables) to (ruleno, nodeId) pairs, indicating the
 *  point of binding (layer (rule) and andor node) of the pattern variable. Given a
 *  pvar (and its lvar) and a ruleno, the pvarmap yields a nodeId, which can be looked
 *  up in an mvarenv to get the associated mvar:
 *    pvar * ruleno -- pvarmap --> nodeId -- mvarenv --> mvar 
 *    (pvarmap * mvarenv) -> pvar * ruleno -> mvar *)

structure PVM = LV.Map
type pvarmap = (ruleno * nodeId) list PVM.map

val pvarmapRef : pvarmap ref = ref PVM.empty

(* mapVarBindings : nodeId * varBindings -> unit *)
fun mapVarBindings (id: nodeId, varBindings: varBindings): unit =
    let (* val _ = dbsays [">>mapVarBindings, #bindings = ", Int.toString (length varBindings)] *)
	fun bind ((var,ruleno), pvmap) =
	    let val lvar = V.varToLvar var
(*		val _ = dbsays ["mapVarBindings: bind: lv", LV.toString lvar, " at ", Int.toString ruleno] *)
	     in (case M.find (pvmap, lvar)
		   of NONE => M.insert(pvmap, lvar, [(ruleno,id)])
		    | SOME locs => M.insert(pvmap, lvar, (ruleno,id)::locs))
	    end
    in pvarmapRef := foldr bind (!pvarmapRef) varBindings
(*       dbsay "<<mapVarBindings" *)
    end

(* pvarFind : LV.lvar * pvarmap -> (ruleno * nodeId) list option *)
fun pvarFind (pvarmap: pvarmap, lvar : LV.lvar) =
    M.find (pvarmap, lvar)

(* translateAndor : protoAndor * path * ruleset * ty -> andor
   Traverses the andor tree accumulating a bindenv binding all generated paths
   (starting with path as the root) to the associated "live" rules from
   the bindings fields in all the andor tree's nodes.

 * getDefaults: variant list * ruleset -> ruleset
   getDefaults (cases, live):
   take a starting set of "live" rules and successively subtract the rules
   component of each variant in the cases arg. The result is a subset of the
   original "live" ruleset that is disjoint from the rules of each caseAndor.
   Defaults are rules that are "live" at the OR node but not because they
   match the (explicit) discriminant of any variant.
   [Does this mean that they are live because of matching a variable
   (along the path to this node)? YES? (check).]  How, exactly, are default
   rules related to pvar bindings?  Can we get away with ignoring the distinction
   between direct (or simple) variable bindings and as bindings?
   NOTE: the paths to vector "variant" nodes (children of a vector OR node) 
   end in VPI links, but do not include a VLEN link, while the path of the
   vector OR node itself terminates in a VLEN link.
*)

(* translateAndor : protoAndor * ruleset * T.ty -> andor *)
fun translateAndor (ANDs {bindings, children}, live, ty) =
      let val id = newId ()
	  val childrenTys = TU.destructRecordTy ty
	  val children' = ListPair.map (fn (pa, ty) => translateAndor (pa, live, ty))
				       (children, childrenTys)
       in mapVarBindings (id, bindings);
          AND {id = id, children = children'}
      end
  | translateAndor (ORs {bindings, cases, sign}, live, ty) =
      let val id = newId ()
	  fun getDefaults (nil, live) = live
	    | getDefaults ((_,rules,_)::rest, live)  =
	        getDefaults (rest, RS.difference (live, rules))
	  val defaults = getDefaults (cases, live) (* subset of live *)
	  fun transCase scase = translateCase (scase, live, defaults, ty)
	  val cases' = map transCase cases
	  val _ = if !debugging
		  then (say ("translateAndor: id = " ^ Int.toString id);
			say "\n  live = ";
		        ppRules live;
		        say "  defaults = ";
		        ppRules defaults)
		  else ()
       in mapVarBindings (id, bindings);
	  OR {id = id, sign = sign, defaults = defaults, cases = cases'}
      end
  | translateAndor (VARs {bindings}, live, ty) =
      let val id = newId ()
       in mapVarBindings (id, bindings);
	  VAR {id = id, ty = ty}
      end

(* translateCase : protoVariant * ruleset * ruleset * T.ty -> variant *)
and translateCase ((con, caserules, subcase), live, defaults, ty) =
    let val caseLive = RS.intersection (caserules, live)
	val stillLive = RS.intersection (RS.union (caserules, defaults), live)
	val _ =  if !debugging
		 then (say "translateCase: con = ";
		       ppCon con;
		       say "  caseLive = "; ppRules caseLive;
		       say "  stillLive = "; ppRules stillLive)
		 else ()
      in case (con, subcase)
	  of (VLENcon (k, elemty), VEC elements) =>
	     let val elements' = map (fn pa => translateAndor (pa, stillLive, elemty)) elements
	      in (con, caseLive, VEC elements')
	     end
	   | (DATAcon (dcon, _), DCON pandor) => (* non-constant datacon *)
             (con, caseLive,
	      DCON (translateAndor (pandor, stillLive, TU.destructDataconTy (ty, dcon))))
	   | (_, CONST) => (* con should be constant, not checked *)
	     (con, caseLive, CONST)
	   | _ => bug "translateCase: inconsistent cases"
      end

(* makeAndor : andor * ruleset -> andor *)
fun makeAndor (andor, patty, allRules) =
      (pvarmapRef := M.empty;  (* reset pvarmap *)
       idcount := 0;           (* reset idcount *)
       (translateAndor (andor, allRules, patty), !pvarmapRef, !idcount))

end (* local *)
end (* structure Andor2 *)
