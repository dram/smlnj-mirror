(* amd64-ccall.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * AMD64 backend specialized to the "ccall" (default) calling convention.
 *)

structure AMD64CCallBackend =
    BackendFn (
      structure M = MachineCodeGenFn (
          structure MachSpec = AMD64Spec
          val abi_variant = NONE)
      val cproto_conv = "ccall")
