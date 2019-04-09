(*
 * util/abspath.sig: Operations over abstract path names.
 *
 *   Copyright (c) 1997 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature ABSPATH = sig

    type t

    val cur: t
    val dummy: t

    val cwd: unit -> string
    val newcwd: unit -> bool

    val spec: t -> { name: string, rigid: bool, context: t }
    val elab: t -> string
    val compare: t * t -> order

    val native: { context: t, spec: string, rigid: bool } -> t
    val standard: { context: t, spec: string, rigid: bool } -> t

    val current: { name: string, rigid: bool } -> t
    val rigidcur: string -> t

    val splitDirFile: t -> { dir: t, file: string }
    val joinDirFile: { dir: t, file: string } -> t
    val file: t -> string
    val dir: t -> t

    val splitBaseExt: t -> { base: t, ext: string option }
    val joinBaseExt: { base: t, ext: string option } -> t
    val base: t -> t
    val ext: t -> string option
    val extendExt: { path: t, ext: string, sep: string } -> t

    val exists: t -> bool

    val modTime: t -> Time.time

    (* the open?Out functions automagically create any necessary directories *)
    val openTextIn: t -> TextIO.instream
    val openTextOut: (string -> unit) -> t -> TextIO.outstream
    val openBinIn: t -> BinIO.instream
    val openBinOut: (string -> unit) -> t -> BinIO.outstream

end
