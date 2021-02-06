(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* ltydict.sml *)

signature LTYDICT =
sig 
  type tyc = Lty.tyc
  type lty = Lty.lty

  val tmemo_gen : {tcf: (tyc -> tyc) -> (tyc -> tyc),
                   ltf: ((tyc -> tyc) * (lty -> lty)) -> (lty -> lty)} 
	       -> {tc_map: tyc -> tyc,
		   lt_map: lty -> lty}

  val wmemo_gen : {tc_wmap : ((tyc -> tyc) * (tyc -> tyc)) -> (tyc -> tyc),
                   tc_umap : ((tyc -> tyc) * (tyc -> tyc)) -> (tyc -> tyc),
                   lt_umap : ((tyc -> tyc) * (lty -> lty)) -> (lty -> lty)}
               -> {tc_wmap : tyc -> tyc,
                   tc_umap : tyc -> tyc, 
                   lt_umap : lty -> lty}

end (* signature LTYDICT *)

structure LtyDict : LTYDICT = 
struct 

type tyc = Lty.tyc
type lty = Lty.lty

structure TcDict = RedBlackMapFn(struct
                                   type ord_key = tyc
				   val compare = Lty.tc_cmp
			         end)

structure LtDict = RedBlackMapFn(struct
                                   type ord_key = lty
				   val compare = Lty.lt_cmp
			         end)

(* tmemo_gen called once in function tnarrow_gen in LtyExtern.
 * tnarrow_gen is called once in FLINT/reps/reify.sml *)
fun tmemo_gen {tcf : (tyc -> tyc) -> (tyc -> tyc),
	       ltf : ((tyc -> tyc) * (lty -> lty)) -> (lty -> lty)} =
  let val tycDictR = ref (TcDict.empty)
      val ltyDictR = ref (LtDict.empty)

      fun tc_look tyc = 
          (case TcDict.find(!tycDictR, tyc)
             of SOME x => x
              | NONE => 
                let val x = (tcf tc_look) tyc
                 in tycDictR := TcDict.insert(!tycDictR, tyc, x);
		    x
                end)

      and lt_look lty = 
          (case LtDict.find(!ltyDictR, lty)
             of SOME x => x
              | NONE => 
                let val x = ltf (tc_look, lt_look) lty
		 in ltyDictR := LtDict.insert(!ltyDictR, lty, x);
		    x
                end)

   in {tc_map=tc_look, lt_map=lt_look}
  end (* tmemo_gen *)


(* Obsolete: folded into definition of typeWrapGen in LtyExtern
(* wmemo_gen called once in function twrap_gen in LtyExtern; 
 * twrap_gen is called once in FLINT/reps/wrapping.sml *)
fun wmemo_gen {tyc_wmap : (tyc -> tyc) * (tyc -> tyc) -> (tyc -> tyc),
               tyc_umap : (tyc -> tyc) * (tyc -> tyc) -> (tyc -> tyc),
               lty_umap : (tyc -> tyc) * (lty -> lty) -> (lty -> lty)} = 
  let 

      fun tcw_look (tyc: tyc) : tyc = 
	  (case TcDict.find(!tycWrapMap, tyc)
	     of SOME x => x
	      | NONE => 
		let val x = tyc_wmap (tcw_look, tcu_look) tyc
		 in tycWrapMap := TcDict.insert(!tycWrapMap, tyc, x);
		    x
		end)
 
      and tcu_look tyc = 
	  (case TcDict.find(!tycUnwrapMap, tyc)
	    of SOME x => x
	     | NONE => 
		 let val x = tyc_umap (tcw_look, tcu_look) tyc
		  in tycUnwrapMap := TcDict.insert(!tycUnwrapMap, tyc, x);
		     x
		 end)

      and ltu_look lty = 
	  (case LtDict.find(!ltyUnwrapMap, lty)
	    of SOME x => x
	     | NONE => 
		 let val x = lty_umap (tcu_look, ltu_look) lty
		  in ltyUnwrapMap := LtDict.insert(!ltyUnwrapMap, lty, x);
		     x
		 end)

   in {tc_wmap=tcw_look, tc_umap=tcu_look, lt_umap=ltu_look}
  end (* wmemo_gen *)
*)
end (* structure LtyDict *)
