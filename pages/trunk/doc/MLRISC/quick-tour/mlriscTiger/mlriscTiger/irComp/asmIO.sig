signature ASM_IO = sig
  val globl : string -> string		(* declare global symbol *)
  val enter : string -> string		(* entry point to symbol *)
  val endf : string -> string		(* end point for symbol *)
  val label : Label.label -> string	(* declare label *)
  val ascii : string -> string		(* layout string *)
  val dotData : unit -> string		(* begin .data *)
  val dotText : unit -> string		(* begin .text *)
  val dotWord : int -> string		(* layout integer constant *)
end
