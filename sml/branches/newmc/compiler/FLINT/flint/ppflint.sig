(* COPYRIGHT (c) 2020 SML/NJ Fellowship *)
(* ppflint-new.sig -- Signature of new pretty printer (PPFlint) for FLINT IR. *)

signature PPFLINT =
sig

    (* pretty printing functions *)

    val ppFKind : PrettyPrint.stream -> FLINT.fkind  -> unit
    val ppRKind : PrettyPrint.stream -> FLINT.rkind  -> unit
    val ppCon   : PrettyPrint.stream -> FLINT.con    -> unit
    val ppValue : PrettyPrint.stream -> FLINT.value  -> unit  (* was printSval *)
    val ppFundec: PrettyPrint.stream -> FLINT.fundec -> unit  (* also takes FLINT.prog *)

    (* "top-level" printing functions *)

    val ppLexp : PrettyPrint.stream -> FLINT.lexp -> unit
      (* controlled by Control.FLINT.printDepth and Control.FLINT.lineWidth *)
    val ppLexpLimited : PrettyPrint.stream -> FLINT.lexp * int -> unit
      (* controlled by Control.FLINT.lineWidth *)
    val ppProg : PrettyPrint.stream -> FLINT.prog -> unit
      (* controlled by Control.FLINT.printDepth and Control.FLINT.lineWidth *)
    val ppProgLimited : PrettyPrint.stream -> FLINT.prog * int -> unit
      (* controlled by Control.FLINT.lineWidth *)

    val lvarToStringRef : (FLINT.lvar -> string) ref
      (* controls printing of lvars; defaults to LV.lvarName *)

    val valueToString : FLINT.value -> string

end (* signature PPFLINT *)
