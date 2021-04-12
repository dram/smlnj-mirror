(* vartypes.sml *)

(* given the type of the lhs (patterns) of a match, and an AND-OR 
tree for the patterns, produce a map from vars to their types.

If the lhs type contains uninstantiated, generalizable metavariables,
instantiate them with new "real" tyvars and return those tyvars to
be abstracted over in mc-code.sml.
*)

structure VarTypes =
struct

local
    structure VM = VarMap
    open Types TypesUtil
in

val tymap = ref (VM.empty)

(* variantTy : key * ty -> ty *)
(* given the type ty of an OR node, derive the type of the variant 
 *  andor *)
fun variantTy (key, ty) =
    case key
      of D(dcon,_) =>
         let val DCon{ty=dconty,...} = dcon
	 in case ty
	      of  CONty(dt, argtys) =>
		    let val insty = intantiatePoly(dconty, argtys)
		    in ifIsFunTy insty
		       then domainTy(insty)
		       else insty
		    end
	        | _ => bug "variantTy"
	 end
        (* get polymorphic type of dcon (owner dt)
            ty will be CONty(dt args)
	    use args to instantiate polymorphic dcon type
	    get domain of instantiated dcon type *)

fun mapTypes (ty, andor) : ty VM.map =
    case (andor, ty)
     of (AND{lvar, children,...}, CONty(RECORDtyc _, argtys)) =>
	  ListPair.app mapTypes (children, argtys)
      | (OR{lvar, variants,...}, ty) => 
	  (tymap := VM.insert (!tymap, lvar, ty);
	   List.app
	     (fn (key, andor') =>
		 let val ty' = variantTy(key, ty)
		  in mapTypes (andor', ty')
		 end)
	     variants)
      | SINGLE (dcon, arg) =>
	  mapTypes(arg, dconDomainTy(dcon,ty))
      | LEAF => ()  (* no lvar to map *)
      | VARS {lvar, vars = (VALvar{ty, ...}, _) :: _ =>
	  tymap := insert (!tymap, lvar, ty)

end (* local *)
end (* structure VarTypes *)
