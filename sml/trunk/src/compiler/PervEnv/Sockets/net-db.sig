(* net-db-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature NET_DB =
  sig
    eqtype net_addr
    type addr_family
    type entry
    val name     : entry -> string
    val aliases  : entry -> string list
    val addrType : entry -> addr_family
    val addr     : entry -> net_addr
    val getByName : string -> entry option
    val getByAddr : (net_addr * addr_family) -> entry option

    val scan       : (char, 'a) StringCvt.reader -> (net_addr, 'a) StringCvt.reader
    val fromString : string -> net_addr option
    val toString   : net_addr -> string
  end

(*
 * $Log: net-db-sig.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:24  george
 *   Version 109.24
 *
 *)
