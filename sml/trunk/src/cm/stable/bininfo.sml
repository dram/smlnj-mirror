(*
 * Bundling information pertaining to the member of a stable group.
 *   - only includes information that does not require running
 *     the machine-dependent part of the compiler
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature BININFO = sig

    type info
    type ord_key = info
    type complainer = GenericVC.ErrorMsg.complainer
    type region = GenericVC.SourceMap.region

    val new : { group: SrcPath.t,
	        mkStablename: unit -> string,
	        error: complainer,
		spec: string,
		offset: int,
		share: bool option } -> info

    val compare : info * info -> order
    val describe : info -> string
    val offset : info -> int
    val group : info -> SrcPath.t
    val stablename : info -> string
    val share : info -> bool option
    val error : info -> complainer
end

structure BinInfo :> BININFO = struct

    type complainer = GenericVC.ErrorMsg.complainer
    type region = GenericVC.SourceMap.region

    datatype info =
	INFO of { group: SrcPath.t,
		  mkStablename: unit -> string,
		  spec: string,
		  offset: int,
		  share: bool option,
		  error: complainer }

    type ord_key = info

    val new = INFO

    fun compare (INFO i, INFO i') =
	case Int.compare (#offset i, #offset i') of
	    EQUAL => SrcPath.compare (#group i, #group i')
	  | unequal => unequal

    fun describe (INFO { group, spec, offset, ... }) =
	concat [SrcPath.descr group, "@", Int.toString offset, "(", spec, ")"]

    fun group (INFO { group = g, ... }) = g
    fun offset (INFO { offset = os, ... }) = os
    fun share (INFO { share = s, ... }) = s
    fun stablename (INFO { mkStablename = msn, ... }) = msn ()

    fun error (INFO { error = e, ... }) = e
end
