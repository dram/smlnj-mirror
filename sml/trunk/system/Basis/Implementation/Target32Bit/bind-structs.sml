(* bind-structs.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Basis structure bindings for 32-bit targets.  Note that many of these
 * are the same on 64-bit targets.
 *)

structure LargeIntImp : INTEGER = IntInfImp
structure LargeRealImp = Real64Imp
structure FixedIntImp = Int32Imp
structure LargeRealImp = Real64Imp
structure LargeWordImp = Word32Imp
structure Math = Math64
structure RealImp = Real64Imp
structure SysWordImp = Word32Imp
