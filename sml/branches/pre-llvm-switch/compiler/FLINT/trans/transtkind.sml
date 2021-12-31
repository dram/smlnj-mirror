(* transtkind.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TransTKind =
  struct

(* translation from the front-end simplified version of tkind (TKind.tkind) to
 * PLambdaType.tkind *)

    local
      structure TK = TKind
    in

  (* trans : TKind.tkind -> PlambdaType.tkind *)
    fun trans (TK.TKCint i) = Lty.tkc_int i
      | trans (TK.TKCfun (args,res)) = Lty.tkc_fun (map trans args, trans res)
      | trans (TK.TKCseq tks) = Lty.tkc_seq(map trans tks)

    end

end (* structure TransTKind *)
