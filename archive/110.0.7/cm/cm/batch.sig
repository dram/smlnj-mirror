(*
 * cm/batch.sig: layout of batch-compiler structure
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature BATCH = sig

    structure CM: FULL_CM
    val version: string
    val make': string option -> unit
    val make: unit -> unit

end

(* the functor will appear in CMR, so here is its signature: *)
funsig CMB_FUN (structure Compiler: COMPILER
		val version: string
		val hostcpun: string
		val hostosn: string
		val targetosn: string) = BATCH
