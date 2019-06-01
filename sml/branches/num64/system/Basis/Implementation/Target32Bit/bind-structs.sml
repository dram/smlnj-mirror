(* bind-structs.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Basis structure bindings for 32-bit targets.  Note that many of these
 * are the same on 64-bit targets.
 *)

structure FixedIntImp = Int64Imp
structure LargeIntImp : INTEGER = IntInfImp

structure LargeWordImp = Word64Imp
structure SysWordImp = Word32Imp

structure RealImp = Real64Imp
structure LargeRealImp = Real64Imp
structure Math = Math64
