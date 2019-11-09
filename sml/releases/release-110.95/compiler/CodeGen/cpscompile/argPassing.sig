(* argPassing.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * parameter passing convention for standard or known functions.
 *)

signature ARG_PASSING =
  sig
    structure T : MLTREE
    val standard : {fnTy: CPS.cty, vfp:bool, argTys: CPS.cty list} -> T.mlrisc list
    val fixed : {argTys:CPS.cty list, vfp:bool} -> T.mlrisc list
  end
