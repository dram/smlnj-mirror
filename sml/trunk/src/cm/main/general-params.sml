(*
 * "General" parameters that may differ from invocation to invocation of
 * CM.  The "info" type bundles them up so they can be passed around
 * more conveniently.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure GeneralParams = struct

    type param = { pcmode : PathConfig.mode,
		   fnpolicy: FilenamePolicy.policy,
		   symval: string -> { get: unit -> int option,
				       set: int option -> unit },
		   keep_going: bool,
		   corenv: GenericVC.Environment.environment }

    type info = { param: param,
		  groupreg: GroupReg.groupreg,
		  errcons: PrettyPrint.ppconsumer }
end
