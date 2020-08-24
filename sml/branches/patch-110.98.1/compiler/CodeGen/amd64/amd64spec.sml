(* amd64spec.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
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
    val initialSpillOffset = 80	(* offset from %rsp (or vfp) *)
    val startgcOffset = 64

    val numRegs = 10	(* length AMD64CpsRegs.miscregs + 3 *)
    val numArgRegs = 4 (* non-callee-save misc regs *)
    val numFloatRegs = 16
    val numFloatCalleeSaves = 0

    val constBaseRegOffset = 0

    val ML_STATE_OFFSET = 176
    val VProcOffMSP = 8
    val InMLOffVSP = 8
    val LimitPtrMaskOffVSP = 200

  end
