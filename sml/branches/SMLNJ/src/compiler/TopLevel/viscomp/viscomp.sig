(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* viscomp.sig *)

signature PRINTHOOKS =
sig
  (* all output goes to Control.Print.out *)
  val prAbsyn : StaticEnv.staticEnv -> Absyn.dec -> unit
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
  structure Environment : ENVIRONMENT = CMEnv.Env
  structure CoerceEnv : COERCE_ENV
  structure EnvRef : ENVREF
  structure ModuleId : MODULE_ID
  structure CMStaticEnv : CMSTATICENV
  structure Profile : PROFILE
  structure Binfile: BINFILE
  structure CMSA: CMSA
  structure PersStamps : PERSSTAMPS
  structure PrettyPrint : PRETTYPRINT
  structure PPTable : 
   sig
    val install_pp : string list -> 
                      (PrettyPrint.ppstream -> 'a -> unit) -> unit
   end
  structure Ast : AST
  structure Compile : COMPILE
  structure Interact : INTERACT
(*
  structure AllocProf : sig val reset : unit -> unit
			    val print : outstream -> unit
			end
*)
  structure PrintHooks : PRINTHOOKS
  val version : {
          system : string,      	(* the system title *)
	  version_id : int list,	(* the version number *)
          date : string         	(* date of creation *)
	}
  val banner : string
  val architecture: string
end  


(*
 * $Log$
 *)
