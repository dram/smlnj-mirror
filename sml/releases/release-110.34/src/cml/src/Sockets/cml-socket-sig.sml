(* cml-socket-sig.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This signature extends the SML Basis SOCKET signature with event
 * constructors for the input operations and accept.
 *)

signature CML_SOCKET =
  sig
    type 'a event = 'a CML.event

    include SOCKET

    val connectEvt : (('a, 'b) sock * 'a sock_addr) -> unit event

    val acceptEvt :
	  ('a, passive stream) sock
	    -> (('a, active stream) sock * 'a sock_addr) event

  (* Sock input event constructors *)
    val recvVecEvt :
	  (('a, active stream) sock * int) -> Word8Vector.vector CML.event
    val recvArrEvt :
	  (('a, active stream) sock * Word8Array.array buf) -> int CML.event
    val recvVecEvt' :
	  (('a, active stream) sock * int * in_flags)
	    -> Word8Vector.vector CML.event
    val recvArrEvt' :
	  (('a, active stream) sock * Word8Array.array buf * in_flags)
	    -> int CML.event
    val recvVecFromEvt :
	  (('a, dgram) sock * int)
	    -> (Word8Vector.vector * 'b sock_addr) CML.event
    val recvArrFromEvt :
	  (('a, dgram) sock * {buf : Word8Array.array, i : int})
	    -> (int * 'a sock_addr) CML.event
    val recvVecFromEvt' :
	  (('a, dgram) sock * int * in_flags)
	    -> (Word8Vector.vector * 'b sock_addr) CML.event
    val recvArrFromEvt' :
	  (('a, dgram) sock * {buf : Word8Array.array, i : int} * in_flags)
	    -> (int * 'a sock_addr) CML.event

  end
