(* bind-toplevel.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file binds the top-level identifiers from SML Standard Library in
 * the pervasive top-level environment.
 *
 *)

open General


(* top-level types *)

datatype bool = datatype bool
datatype list = datatype list
datatype ref = datatype PrimTypes.ref


(* top-level value identifiers *)

(* Misc *)
val op =  : ''a * ''a -> bool = InlineT.=
val op <> : ''a * ''a -> bool = InlineT.<>
val vector = Vector.fromList

(* Bool *)
val not = Bool.not

(* Option *)
datatype option = datatype Option.option
exception Option = Option.Option
val getOpt = Option.getOpt
val isSome = Option.isSome
val valOf = Option.valOf

(* Int *)
type int = Int.int

(* Word *)
type word = Word.word

(* Real *)
type real = Real.real
val real = Real.fromInt
val trunc = Real.trunc
val floor = Real.floor
val ceil = Real.ceil
val round = Real.round


(* List *)
exception Empty = List.Empty
val null   = List.null
val hd     = List.hd
val tl     = List.tl
val length = List.length
val rev    = List.rev
val (op @) = List.@
val app    = List.app
val map    = List.map
val foldr  = List.foldr
val foldl  = List.foldl

(* Array *)
type 'a array = 'a Array.array

(* Vector *)
type 'a vector = 'a Vector.vector

(* Char *)
type char = Char.char
val ord = Char.ord
val chr = Char.chr

(* String *)
type string = String.string
val size	= String.size
val str		= String.str
val concat	= String.concat
val implode	= String.implode
val explode	= String.explode
val substring	= String.substring
val (op ^)	= String.^

(* Substring *)
type substring = Substring.substring

(* I/O *)
val print = PrintHook.print


(*
 * $Log: bind-toplevel.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.9  1997/11/25 22:40:17  jhr
 *   SML Basis changes: removed <> and = from General and changed the type
 *   of General.before.
 *
 * Revision 1.8  1997/08/27  00:25:26  jhr
 *   Fixed bug 1159 (extraneous types in top-level environment).
 *
 * Revision 1.7  1997/07/15  15:52:16  dbm
 *   Correct spelling in a comment.
 *
 * Revision 1.6  1997/04/17  12:25:29  george
 *   Replaced several opens with datatype = datatype bindings -- jhr
 *
 * Revision 1.5  1997/02/26  21:00:04  george
 *    Defined a new top level Option structure. All 'a option related
 *    functions have been moved out of General.
 *
 * Revision 1.4  1997/02/18  14:17:51  george
 *   Spurious types like word8, word32 from PrimTypes, leak into the top
 *   level environment. They will continue to do so until one can write
 *   done a definition for the ref data constructor. The mechanism
 *   to thin down PrimTypes is in place.
 *
 * Revision 1.3  1997/01/29  14:51:24  jhr
 * Added hook to allow dynamic rebinding of top-level print function.
 *
 * Revision 1.2  1997/01/28  23:13:27  jhr
 * Added List.Empty to top-level as per SML'96 basis library.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:13  george
 *   Version 109.24
 *
 *)
