(* tags-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * NOTE: eventually, this should move out of the boot directory and into
 * the machine specification.
 *)

signature TAGS =
  sig
    type tag
    val width_tags : int (* number of bits to hold a tag *)
    val power_tags : int (* 2 ^ width_tags *)
    val max_length : int (* one greater than max length value *)
  (* tag values *)
    val tag_record : tag
    val tag_pair : tag
    val tag_array : tag
    val tag_string : tag
    val tag_word8array : tag
    val tag_reald : tag
    val tag_realdarray : tag
    val tag_cont : tag
    val tag_block : tag
    val tag_variant : tag (* currently not used *)
    val tag_special : tag
    val tag_backptr : tag
  (* build a descriptor from a tag and length *)
    val make_desc : (int * tag) -> int
  (* fixed descriptors *)
    val desc_pair : int
    val desc_reald : int
  (* special descriptors *)
    val desc_special : int
    val special_evaled_susp : int
    val special_unevaled_susp : int
    val special_weak : int
    val special_nulled_weak : int
  end

