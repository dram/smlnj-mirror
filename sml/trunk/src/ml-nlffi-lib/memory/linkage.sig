(*
 * This module defines a high-level interface for dlopen.
 *   While addresses (those obtained by applying function "addr" below
 *   or addresses derived from those) will not remain valid across
 *   export{ML,Fn}/restart, handles *will* stay valid.
 *
 * (C) 2001, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume
 *)
signature DYN_LINKAGE = sig

    exception DynLinkError of string

    type lib_handle		(* handle on dynamically linked library (DL) *)
    type addr_handle		(* handle on address obtained from a DL *)

    val main_lib : lib_handle	(* the runtime system itself *)

    (* link new library and return its handle *)
    val open_lib : { name: string, lazy: bool, global: bool } -> lib_handle

    (* get the address handle of a symbol exported from a DL *)
    val lib_symbol : lib_handle * string -> addr_handle

    (* fetch the actual address from an address handle; the value obtained
     * is not valid across export{ML,Fn}/resume cycles *)
    val addr : addr_handle -> Word32.word

    (* unlink previously linked DL; this immediately invalidates all
     * symbol addresses and handles associated with this library *)
    val close_lib : lib_handle -> unit
end
