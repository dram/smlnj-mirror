(* tags.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 *
 *)


structure Tags : TAGS =
  struct
    val (op -) = InlineT.DfltInt.-

    val itow = Word.fromInt
    val wtoi = Word.toIntX

    datatype tag = TAG of int
  (* taken from runtime/tags.h *)
    val width_tags = 6  (* 4 tag bits plus "10" *)
  (* one greater than the maximum length field value (sign should be 0) *)
    val max_length = wtoi(Word.<<(0w1, Word.-(0w31, itow width_tags)))
    val power_tags = 0x40  (* 1 << 6 *)
    local
      fun tagXLen(lTag, n) = TAG(wtoi(Word.orb(lTag, Word.<<(n, 0w2))))
    in
      fun tagWLen n  = tagXLen(0wx22, n)
      fun tagWOLen n = tagXLen(0wx02, n)		  
    end
    val tag_record		= tagWLen 0w0
    val tag_array		= tagWLen 0w1
    val tag_string		= tagWLen 0w2
    val tag_word8array		= tagWLen 0w4
    val tag_realdarray		= tagWLen 0w5
    val tag_cont                = tagWLen 0w6
    val tag_block               = tagWLen 0w7
    val tag_pair		= tagWOLen 0w0
    val tag_reald		= tagWOLen 0w1
    val tag_variant		= tagWOLen 0w3 (* currently not used *)
    val tag_special		= tagWOLen 0w4
    val tag_backptr		= tagWOLen 0w5
  (* build a descriptor from a tag and length *)
    fun make_desc (len, TAG t) = 
      wtoi(Word.orb(Word.<<(itow len, itow width_tags), itow t))
  (* fixed descriptors *)
    val desc_pair = make_desc(2, tag_pair)
    val desc_reald = make_desc(2, tag_reald)
  (* special descriptors *)
    val desc_special = make_desc(0, tag_special)
    val special_unevaled_susp	= 0
    val special_evaled_susp	= 1
    val special_weak		= 2
    val special_nulled_weak	= 3
  end (* structure Tags *)

(*
 * $Log: tags.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:39  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/12/03 21:20:32  dbm
 *   Name changes: bytearray -> word8array; realarray -> real64array.
 *
 * Revision 1.2  1997/06/30  19:36:22  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:17  george
 *   Version 109.24
 *
 *)
