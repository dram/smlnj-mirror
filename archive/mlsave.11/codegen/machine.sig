(* machine.sig *)

signature MACHINE =
  sig type Label
      val offset : int ref
      val newlabel : unit -> Label
      val path : int list -> unit		(* + *)
      val const : int -> unit			(* + *)
      val label : Label -> unit			(* + *)
      val squeeze : unit -> unit		(* - *)
      val apply : unit -> unit			(* - *)
      val applyknown : Label -> unit		(* - *)
      val tail : unit -> unit			(* - *)
      val tailrecur : int -> unit
      val startswitch : unit -> unit		(* - *)
      val testboxed : Label -> unit
      val boxed : Label * Label -> unit
      val endboxed: Label -> unit
      val gettag : unit -> unit
      val testcase : Label -> unit		(* - *)
      val testcase_int: int*Label -> unit
      val testcase_real: Label*Label -> unit
      val testcase_string: Label*Label*int -> unit
      val endcase : Label*Label -> unit		(* - *)
      val endswitch : Label -> unit
      val nextelem : int -> unit		(* - *)
      val startalloc : int -> unit
      val endrecord : unit -> unit		(* + *)
      val endclosure : unit -> unit		(* + *)
      val select : int -> unit
      val starthandle : Label*Label -> unit	(* + + *)
      val midhandle : Label*Label -> unit	(* - - *)
      val endhandle : Label*Label -> unit
      val raisexn : unit -> unit
      val definelabel : Label -> unit
      val return : unit -> unit
      val stringconst : string*Label -> unit
      val realconst : string*Label -> unit
      exception Notprim
      val canapp : int -> int
      val primapp : int -> unit
      val profile : (int * int) -> unit
 end;

signature MACHINECODE =
sig
    structure M : MACHINE
    val assemble : unit -> (int * ((int -> unit)->unit))
end

signature ASSEMBLYCODE =
sig
    structure M : MACHINE
    val outfile : outstream ref
end
