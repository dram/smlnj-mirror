(* amd64-frame-layout.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module defines symbolic names for the stack-pointer offsets
 * used to access various bits of information from the SML stack
 * frame.  See dev-notes/amd64-stack-frame.numbers for the layout.
 *)

structure AMD64FrameLayout =
  struct

  (* offset of register-spill area *)
    val spillAreaOffset = 0

  (* size in bytes of spill area *)
    val spillAreaSzb = 8 * 1024

  (* location of exception pointer *)
    val exnPtrOffset = 8224

  (* location of var pointer *)
    val varPtrOffset = 8232

  (* location of ML State pointer *)
    val mspOffset = 8200

  (* location of `saveregs` address *)
    val saveregsOffset = 8240

  (* location of the base pointer *)
    val basePtrOffset = 8216

  (* location to save the GC return address (gc-link) *)
    val gcLinkOffset = 8208

  (* location of double-precision sign-bit mask *)
    val signBitOffset = 8248

  (* location of double-precision negated sign-bit mask *)
    val negSignBitOffset = 8256

  end
