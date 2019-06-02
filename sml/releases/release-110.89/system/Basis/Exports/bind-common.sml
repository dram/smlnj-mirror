(* bind-common.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Exported Basis Library modules that are common to both 32-bit and
 * 64-bit targets.
 *)

structure General =
  struct
    open General
    open ExnName
  end

structure SMLofNJ : SML_OF_NJ =
  struct
    open SMLofNJ
    val exportML = Export.exportML
    val exportFn = Export.exportFn
    structure Cont = Cont
    structure IntervalTimer = IntervalTimer
    structure Internals = Internals
    structure SysInfo = SysInfo
    structure Weak = Weak
    structure Susp = Susp
  end

structure CharArray : MONO_ARRAY = Text.CharArray
structure CharVector : MONO_VECTOR = Text.CharVector
structure Char : CHAR = Text.Char

structure String : STRING_2015 = Text.String
structure Substring : SUBSTRING = Text.Substring

structure Int = IntImp
structure Int32 = Int32Imp
structure Int64 = Int64Imp
structure IntInf = IntInfImp
structure FixedInt = FixedIntImp
structure LargeInt = LargeIntImp

structure Real = RealImp
structure Real64 = Real64Imp
structure LargeReal = LargeRealImp
structure RealArray = Real64Array
structure RealVector = Real64Vector
structure RealArraySlice = Real64ArraySlice
structure RealVectorSlice = Real64VectorSlice

structure Word = WordImp
structure Word8 = Word8Imp
structure Word32 = Word32Imp
structure Word64 = Word64Imp
structure LargeWord = LargeWordImp

structure OS = OSImp
structure Position = PositionImp
structure Socket = SocketImp
structure SysWord = SysWordImp
structure Time : TIME = TimeImp
