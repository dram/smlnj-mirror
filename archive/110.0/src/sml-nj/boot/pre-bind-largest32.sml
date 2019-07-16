(* pre-bind-largest32.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Bindings of Int, LargeInt, Word, LargeWord and SysWord
 * structures for 32-bit implementations.
 *
 *)

structure Int = Int31
structure Word = Word31
structure LargeInt = Int32
structure LargeWord = Word32
structure Real = Real64
structure LargeReal = Real64
structure SysWord = Word32
structure Position = Int31

(*
 * $Log: pre-bind-largest32.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:38  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:16  george
 *   Version 109.24
 *
 *)
