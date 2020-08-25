(* stamp.sml
 *
 * COPYRIGHT (c) 2016 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Stamps are locally unique identifiers used in the compiler to
 * distinguish different types, variables, etc.  For a given compilation,
 * the stamp assigned to an object is guaranteed to be unique, although
 * an object may have different stamps assigned to it in different compiles.
 *)

structure Stamp :> sig

    type stamp

    val new : unit -> stamp

    val same : (stamp * stamp) -> bool
    val compare : (stamp * stamp) -> order
    val hash : stamp -> word

    val toString : stamp -> string

    structure Set : ORD_SET where type Key.ord_key = stamp
    structure Map : ORD_MAP where type Key.ord_key = stamp
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = stamp

  end =
  struct

    datatype stamp = STAMP of {
	id : Word.word
      }

    val cnt = ref 0w0

    fun new () = let val id = !cnt in cnt := id+0w1; STAMP{id = id} end

    fun same (STAMP{id, ...}, STAMP{id=id', ...}) = (id = id')
    fun compare (STAMP{id, ...}, STAMP{id=id', ...}) = Word.compare(id, id')
    fun hash (STAMP{id, ...}) = id

    fun toString (STAMP{id, ...}) = StringCvt.padLeft #"0" 4 (Word.toString id)

    structure Key =
      struct
	type ord_key = stamp
	val compare = compare
      end

    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)

    structure Tbl = HashTableFn (struct
	type hash_key = stamp
	val hashVal = hash
	val sameKey = same
      end)

  end
