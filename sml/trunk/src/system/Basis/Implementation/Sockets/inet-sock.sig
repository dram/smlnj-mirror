(* inet-sock.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature INET_SOCK =
  sig

    type inet

    type 'sock_type sock = (inet, 'sock_type) Socket.sock
    type 'mode stream_sock = 'mode Socket.stream sock
    type dgram_sock = Socket.dgram sock

    type sock_addr = inet Socket.sock_addr

    val inetAF : Socket.AF.addr_family   (* DARPA internet protocols *)

    val toAddr   : NetHostDB.in_addr * int -> sock_addr
    val fromAddr : sock_addr -> NetHostDB.in_addr * int
    val any  : int -> sock_addr

    structure UDP : sig
	val socket  : unit -> dgram_sock
	val socket' : int -> dgram_sock
      end

    structure TCP : sig
	val socket  : unit -> 'mode stream_sock
	val socket' : int -> 'mode stream_sock
      (* tcp control options *)
	val getNODELAY : 'mode stream_sock -> bool
	val setNODELAY : 'mode stream_sock * bool -> unit
      end
  end
