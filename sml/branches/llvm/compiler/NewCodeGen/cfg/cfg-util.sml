(* cfg-util.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CFGUtil : sig

  (* does a cluster contain any raw C calls? *)
    val hasRCC : CFG.cluster -> bool

    val tyToString : CFG.ty -> string

    val mkVAR : LambdaVar.lvar -> CFG.exp
    val mkLABEL : LambdaVar.lvar -> CFG.exp
    val mkNUM : IntInf.int * int -> CFG.exp
    val mkLOOKER : CFG_Prim.looker * exp list -> CFG.exp
    val mkPURE : CFG_Prim.pure * exp list -> CFG.exp
    val mkSELECT : int * exp -> CFG.exp
    val mkOFFSET : int * exp -> CFG.exp

  end = struct

    structure C = CFG

    fun hasRCC (C.Cluster({hasRCC, ...}, _, _)) = hasRCC

    fun tyToString (CFG.NUMt sz) = "i" ^ Int.toString sz
      | tyToString (CFG.FLTt sz) = "f" ^ Int.toString sz
      | tyToString CFG.PTRt = "ptr"
      | tyToString CFG.LABt = "label"
      | tyToString CFG.TAGt = "int"

  (* helper functions for constructing expression terms *)
    fun mkVAR lv = C.VAR{name = lv}
    fun mkLABEL lv = C.LABEL{name = lv}
    fun mkNUM (n, sz) = C.NUM{iv = n, sz = sz}
    fun mkLOOKER (p, args) = C.LOOKER{oper = p, args = args}
    fun mkPURE (p, args) = C.PURE{oper = p, args = args}
    fun mkSELECT (i, arg) = C.SELECT{idx = i, arg = arg}
    fun mkOFFSET (i, arg) = C.OFFSET{idx = i, arg = arg}

  end
