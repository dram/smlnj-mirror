(*
 * "General" parameters that may differ from invocation to invocation of
 * CM.  The "params" type bundles them up so they can be passed around
 * more conveniently.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure GeneralParams = struct

    type param = { primconf : Primitive.configuration,
		   fnpolicy: FilenamePolicy.policy,
		   groupreg: GroupReg.groupreg,
		   keep_going: bool,
		   pervasive: GenericVC.Environment.environment,
		   corenv: GenericVC.BareEnvironment.staticEnv }

    type info = { param: param, groupreg: GroupReg.groupreg }
end
