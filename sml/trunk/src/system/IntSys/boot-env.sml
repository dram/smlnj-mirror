(* Copyright 1998 by Lucent Technologies *)
(* boot-env.sml *)

(* Run the BootEnvF functor which builds the boot environments.
 *   It is important that this functor is done executing by the time
 *   the code for the InteractiveSystem runs.  Otherwise we would never
 *   be able to get rid of CM/CMB from an interactive heap image.
 *  -M.Blume (6/1998)
 *)
structure BootEnv =
    BootEnvF (datatype envrequest = datatype CM0.envrequest
	      val architecture = Compiler.architecture
	      val cminit = CM0.init
	      val cmbmake = ignore o CMB.make' o SOME)
