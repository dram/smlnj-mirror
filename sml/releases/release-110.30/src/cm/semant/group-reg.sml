(*
 * The "group registry".  CM uses this to remember which groups it is
 * currently working on and what the corresponding input sources are.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature GROUPREG = sig

    type groupreg

    val new : unit -> groupreg
    val register :
	groupreg -> SrcPath.file * GenericVC.Source.inputSource -> unit
    val lookup : groupreg -> SrcPath.file -> GenericVC.Source.inputSource
    val registered : groupreg -> SrcPath.file -> bool
    val error :
	groupreg
	-> SrcPath.file * GenericVC.SourceMap.region
	-> GenericVC.ErrorMsg.complainer
end

structure GroupReg :> GROUPREG = struct

    type groupreg = GenericVC.Source.inputSource SrcPathMap.map ref

    fun new () = ref SrcPathMap.empty : groupreg

    fun register gr (p, s) = gr := SrcPathMap.insert (!gr, p, s)
    fun lookup gr p = valOf (SrcPathMap.find (!gr, p))
	handle Option => raise Fail ("GroupReg.lookup " ^ SrcPath.descr p)
    fun registered gr g = isSome (SrcPathMap.find (!gr, g))
    fun error gr (g, r) = GenericVC.ErrorMsg.error (lookup gr g) r
end
