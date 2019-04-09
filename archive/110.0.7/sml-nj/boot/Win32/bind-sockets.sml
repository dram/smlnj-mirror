(* bind-sockets.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file binds the SML Standard Library signatures and structures that
 * support Berkeley sockets.
 *
 *)

signature NET_HOST_DB = NET_HOST_DB
signature NET_PROT_DB = NET_PROT_DB
signature NET_SERV_DB = NET_SERV_DB
signature SOCKET = SOCKET
signature INET_SOCK = INET_SOCK
signature GENERIC_SOCK = GENERIC_SOCK

structure GenericSock = GenericSock
structure NetHostDB = NetHostDB
structure NetProtDB = NetProtDB
structure NetServDB = NetServDB
structure Socket = Socket
structure INetSock = INetSock

