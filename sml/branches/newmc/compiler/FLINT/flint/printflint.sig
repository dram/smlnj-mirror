(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ppflint.sig -- Pretty printer for Flint IL. *)

signature PRINT_FLINT =
sig
    val printFKind : FLINT.fkind -> unit
    val printRKind : FLINT.rkind -> unit
    val printCon   : FLINT.con -> unit
    val printSval  : FLINT.value -> unit
    val printLexp  : FLINT.lexp -> unit
    val printFundec: FLINT.fundec -> unit
    val printProg  : FLINT.prog -> unit

    val printTyc : LtyExtern.tyc -> unit
    val printLty : LtyExtern.lty -> unit
    val printTycList : LtyExtern.tyc list -> unit
    val printLtyList : LtyExtern.lty list -> unit

    (* defaults to LV.lvarName *)
    val lvarToStringRef  : (FLINT.lvar -> string) ref

    val valueToString : FLINT.value -> string

end (* signature PRINT_FLINT *)
