(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ppflint.sig -- Pretty printer for Flint IL. *)

signature PPFLINT =
sig
    val printFKind : FLINT.fkind -> unit
    val printRKind : FLINT.rkind -> unit
    val printCon   : FLINT.con -> unit
    val printSval  : FLINT.value -> unit
    val printLexp  : FLINT.lexp -> unit
    val printFundec: FLINT.fundec -> unit

end (* signature PPFLINT *)

