structure LexGen =
struct

exception E

fun f () : unit = () handle E => () ;
(*
val a = () handle x => raise x
*)
end
