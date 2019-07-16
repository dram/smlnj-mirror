(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* viscomp.sig *)

signature PRINTHOOKS =
sig
  (* all output goes to Control.Print.out *)
  val prAbsyn : StaticEnv.staticEnv -> Absyn.dec -> unit
  val prLambda : Lambda.lexp -> unit
  val prLamFun : Lambda.lexp -> int -> unit
end


signature VISCOMP = 
sig 
  structure Stats : STATS
  structure Control : CONTROL
  structure Source : SOURCE
  structure SourceMap : SOURCE_MAP
  structure ErrorMsg : ERRORMSG
  structure Symbol : SYMBOL
  structure StaticEnv : STATICENV
  structure DynamicEnv : DYNENV
  structure BareEnvironment : ENVIRONMENT
  structure Environment : ENVIRONMENT = SCEnv.Env
  structure CoerceEnv : COERCE_ENV
  structure EnvRef : ENVREF
  structure ModuleId : MODULE_ID
  structure SCStaticEnv : SCSTATICENV
  structure Profile : PROFILE
  structure CUnitUtil : CUNITUTIL
  structure CMSA: CMSA
  structure PersStamps : PERSSTAMPS
  structure PrettyPrint : PRETTYPRINT
  structure PPTable : sig
      val install_pp : string list -> (PrettyPrint.ppstream -> 'a -> unit) -> unit
    end
  structure Ast : AST
  structure Lambda: sig type lexp end
  structure Compile : COMPILE
  structure Interact : INTERACT
  structure Machm : CODEGENERATOR
(*
  structure AllocProf : sig val reset : unit -> unit
			    val print : outstream -> unit
			end
*)
  structure PrintHooks : PRINTHOOKS
(*  functor Debugger : DEBUGGERFUN *)
  structure Boot : sig val coreEnvRef : SCEnv.Env.environment ref end
  val version : {
          system : string,      	(* the system title *)
	  version_id : int list,	(* the version number *)
          date : string         	(* date of creation *)
	}
  val banner : string
  val architecture: string
end  



(*
 * $Log: viscomp.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/05/20 12:16:43  dbm
 *   SML '97 sharing, where structure.
 *
 * Revision 1.2  1997/04/16  10:32:50  george
 *   Added support for CMSA -- blume
 *
 * Revision 1.1.1.1  1997/01/14  01:38:28  george
 *   Version 109.24
 *
 *)
