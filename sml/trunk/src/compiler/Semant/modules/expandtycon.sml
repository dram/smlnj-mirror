signature EXPAND_TYCON =
sig
  type sigContext = Modules.elements list
  val expandTycon : Types.tycon * sigContext * EntityEnv.entityEnv -> Types.tycon
  val debugging : bool ref
end

structure ExpandTycon : EXPAND_TYCON =
struct

local (* imported structures *)
  structure T = Types
  structure TU = TypesUtil
  structure EP = EntPath
  structure M = Modules
  structure MU = ModuleUtil
in

(* debugging hooks *)
val say = Control.Print.say
val debugging = ref false
fun debugmsg (msg: string) =
      if !debugging then (say msg; say "\n") else ()
fun bug s = ErrorMsg.impossible ("ExpandTycon: " ^ s)

type sigContext = M.elements list

exception OUTER

(* ignoring FCTspec - won't find any types there *)
fun lookEntVar(ev,(_,s as (M.TYCspec{entVar,...} | M.STRspec{entVar,...}))::rest) =
      if EP.eqEntVar(ev,entVar) then SOME s else lookEntVar(ev,rest)
  | lookEntVar(ev,_::rest) = lookEntVar(ev,rest)
  | lookEntVar(ev,nil) = NONE

fun findContext(ev,context as elements0::outer) =
      (case lookEntVar(ev, elements0)
	 of SOME(M.STRspec{sign as M.SIG{elements,...},...}) =>
	     elements::context
	  | NONE => findContext(ev,outer)
	  | _ => bug "findContext - bad element")
  | findContext(ev,nil) = raise OUTER

fun expandTycon(tycon,context,entEnv) =
    let fun expandTycVar(ev,context as elements::outer) : T.tycon =
	      (case lookEntVar(ev, elements)
		 of SOME(M.TYCspec{spec,...}) =>
		     (case spec
			of T.GENtyc _ => spec
			 | T.DEFtyc{stamp,strict,path,tyfun} =>
			     T.DEFtyc{stamp=stamp,strict=strict,path=path,
				      tyfun=expandTyfun(tyfun,context)})
		  | NONE => (* try outer context *)
		     expandTycVar(ev,outer)
		  | _ => bug "expandTycon 1")
	  | expandTycVar(ev,nil) = raise OUTER

	and expandTyc context = 
	     fn (tyc as T.PATHtyc{entPath,...}) =>
	         (expandPath(entPath,context)
		  handle OUTER => (* path outside current signature context *)
		    MU.transTycon entEnv tyc)
	      | tyc => tyc

	and expandTyfun(T.TYFUN{arity,body},context) = 
	     T.TYFUN{arity=arity,
		     body=TU.mapTypeFull (expandTyc context) body}

	and expandPath(ep, context) =
	    (case ep
	       of nil => bug "expandPath 1"
		| ev :: nil =>  (* tycon! *)
		   expandTycVar(ev,context)
		| ev :: rest => (* substructure! *)
		   expandPath(rest,findContext(ev, context)))

     in expandTyc context tycon
    end

end (* local *)
end (* structure ExpandTycon *)

(*
 * $Log: expandtycon.sml,v $
 * Revision 1.3  1997/04/02  04:06:45  dbm
 *   Removed redundant rule in function expandPath.
 *
 * Revision 1.2  1997/02/26  15:38:39  dbm
 * Fix bug 1141.  Added entityEnv parameter to expandTycon and rewrote body of
 * module so that the entityEnv parameter would be used if a path could not be
 * interpreted in the sigContext parameter.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:42  george
 *   Version 109.24
 *
 *)
