(*
 * "Primitive" classes in CM.
 *   - provide access to compiler internals in an orderly fashion
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature PRIMITIVE = sig

    type primitive

    val fromString : string -> primitive option
    val toString : primitive -> string

    val exports: primitive -> SymbolSet.set
end

structure Primitive :> PRIMITIVE = struct

    datatype primitive = CORE | HELPER

    fun fromString "primitive_core" = SOME CORE
      | fromString "primitive_helper" = SOME HELPER
      | fromString _ = NONE

    fun toString CORE = "primitive_core"
      | toString HELPER = "primitive_helper"

    fun exports p = (ignore Dummy.v; SymbolSet.empty)
end
