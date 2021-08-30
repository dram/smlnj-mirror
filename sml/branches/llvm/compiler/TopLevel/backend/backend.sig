(* backend.sig
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature BACKEND =
  sig
    structure Profile : PROFILE
    structure Compile : COMPILE
    structure Interact : INTERACT
    val architecture: string
    val abi_variant: string option
  end
