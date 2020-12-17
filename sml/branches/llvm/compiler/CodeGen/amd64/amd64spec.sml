(* amd64spec.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * See dev-notes/amd64-stack-frame.numbers for stack-frame layout information.
 *)

structure AMD64Spec : MACH_SPEC =
  struct

    structure DMS = DefaultMachSpecFn (
      struct
	val wordByteWidth = 8
	val addressByteWidth = 8
      end)
    open DMS

    val architecture = "amd64"
    val bigEndian = false

  (* spill-area info; this should match the layout in runtime/mach-dep/AMD.prim.asm,
   * which is also documented in dev-info/amd64-stack-frame.numbers.
   *)
    val spillAreaSz = 8192
(* old MLRISC offsets
    val initialSpillOffset = 80	(* offset from %rsp (or vfp) *)
    val startgcOffset = 64	(* stack offset of `saveregs` address *)
*)
(* new LLVM offsets *)
    val initialSpillOffset = 0	(* offset from %rsp (or vfp) *)
    val startgcOffset = 8240	(* stack offset of `saveregs` address *)
(* *)

    val numRegs = 10	(* length AMD64CpsRegs.miscregs + 3 *)
    val numArgRegs = 4 (* non-callee-save misc regs *)
    val numFloatRegs = 16
    val numFloatCalleeSaves = 0

    val constBaseRegOffset = 0

  (* offset of the ML state pointer in the stack frame (relative to %rsp) *)
(* old MLRISC offsets
    val ML_STATE_OFFSET = 8
*)
(* new LLVM offsets *)
    val ML_STATE_OFFSET = 8200
(* *)

  (* offsets in ML state vector *)
    val VProcOffMSP = 8
    val InMLOffVSP = 8
    val LimitPtrMaskOffVSP = 200

  end
