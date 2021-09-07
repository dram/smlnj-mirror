(* arm64-backend.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Arm64Backend =
    BackendFn (
      structure M = MachineCodeGenFn (
          structure MachSpec = Arm64Spec
          val abi_variant = NONE)
      val cproto_conv = "ccall")
