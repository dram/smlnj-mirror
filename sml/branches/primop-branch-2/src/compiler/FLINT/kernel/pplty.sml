(* pplty.sml
 * 
 * (c) 2006 SML/NJ Fellowship
 *
 * Pretty Printer for PLambda types using the new SMLNJ-lib new pretty printer
 *
 *)

structure PPLTy =
struct

local 

    structure LK = LtyKernel
    structure PT = PrimTyc
    structure PP = PrettyPrintNew
    open PPUtilNew
in

fun ppList ppstrm {sep, pp : 'a -> unit} list =
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
    in
	(ppSequence ppstrm
		    {sep = fn ppstrm => (PP.string ppstrm sep;
					 PP.break ppstrm {nsp=1, offset=0}),
		     style = INCONSISTENT,
		     pr = (fn _ => fn elem => 
				      (openHOVBox 1;
				       pps "(";
				       pp elem;
				       pps ")";
				       closeBox()))}
		    list)
    end (* ppList *)

(* ppTKind : tkind -> unit 
 * Print a hashconsed representation of the kind *)
fun ppTKind ppstrm (tk : LK.tkind) =
    let val ppTKind' = ppTKind ppstrm
	val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
	val ppList' = ppList ppstrm
	fun ppTKindI(LK.TK_MONO) = pps "TK_MONO"
	  | ppTKindI(LK.TK_BOX) = pps "TK_BOX"
	  | ppTKindI(LK.TK_FUN (argTkinds, resTkind)) = 
	      (* res_tkind is a TK_SEQ wrapping some tkinds 
	       * These are produced by Elaborate/modules/instantiate.sml 
	       *)
	     (openHOVBox 1;
	      pps "TK_FUN (";
	      ppList' {sep="* ", pp=ppTKind'} argTkinds;
	      ppTKind' resTkind;
	      pps ")";
	      closeBox())
	  | ppTKindI(LK.TK_SEQ tkinds) =
	    (openHOVBox 1;
	     pps "TK_SEQ(";
	     ppList' {sep=", ", pp=ppTKind'} tkinds;
	     pps ")";
	     closeBox())
    in ppTKindI (LK.tk_out tk)
    end (* ppTKind *)

fun tycEnvFlatten(tycenv) = 
    (case LK.tcSplit(tycenv) of
	 NONE => []
       | SOME(elem, rest) => elem::tycEnvFlatten(rest))

fun ppTycEnvElem ppstrm (tycop, i) =
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
    in
	openHOVBox 1;
	pps "(";
	(case tycop of
	     NONE => pps "*"
	   | SOME(tycs) => ppList ppstrm {sep=",", pp=ppTyc ppstrm} tycs);
	pps ", ";
	PP.break ppstrm {nsp = 1, offset=0}; 
	ppi ppstrm i;
	pps ")";
	closeBox()
    end (* function ppTycEnvElem *)

and ppTyc ppstrm (tycon : LK.tyc) =
    (* FLINT variables are represented using deBruijn indices *)
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
					       (* eta-expansion of ppList to avoid 
						  value restriction *) 
	val ppList' : {pp:'a -> unit, sep: string} -> 'a list -> unit = fn x => ppList ppstrm x
	val ppTKind' = ppTKind ppstrm
	val ppTyc' = ppTyc ppstrm
	fun ppTycI (LK.TC_VAR(depth, cnt)) =
	    (pps "TC_VAR(";
	     PP.break ppstrm {nsp=1,offset=1};
	     (* depth is a deBruijn index set in elabmod.sml/instantiate.sml *)
	     pps (DebIndex.di_print depth);
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     (* cnt is computed in instantiate.sml sigToInst or 
	        alternatively may be simply the IBOUND index *)
	     pps (Int.toString cnt);
	     pps ")")
	  (* Named tyc VAR; is actually an lvar *)
	  | ppTycI (LK.TC_NVAR tvar) =
	    (pps "TC_NVAR(";
	     PP.break ppstrm {nsp=1,offset=1};
	     pps (Int.toString tvar);
	     pps ")")
	  | ppTycI (LK.TC_PRIM primtycon) =
	    (pps "TC_PRIM(";
	     PP.break ppstrm {nsp=1,offset=0};
	     pps (PT.pt_print primtycon);
	     pps ")")
	  | ppTycI (LK.TC_FN (argTkinds, resultTyc)) =
	    (openHOVBox 1;
	     pps "TC_FN(";
	     PP.break ppstrm {nsp=1,offset=1};
	     ppList' {sep="* ", pp=ppTKind'} argTkinds;
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppTyc' resultTyc;
	     pps ")";
	     closeBox())
	  | ppTycI (LK.TC_APP(contyc, tys)) =
	    (openHOVBox 1;
	     pps "TC_APP(";
	     PP.break ppstrm {nsp=1,offset=1};
	     ppTyc' contyc;
	     pps ",";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppList' {sep="* ", pp=ppTyc'} tys;
	     pps ")";
	     closeBox())
	  | ppTycI (LK.TC_SEQ tycs) =
	    (openHOVBox 1;
	     pps "TC_SEQ(";
	     PP.break ppstrm {nsp=1,offset=1};
	     ppList' {sep=", ", pp=ppTyc'} tycs;
	     pps ")";
	     closeBox())
	  | ppTycI (LK.TC_PROJ(tycon, index)) =
	    (openHOVBox 1;
	     pps "TC_PROJ(";
	     PP.break ppstrm {nsp=1,offset=1};
	     ppTyc' tycon;
	     pps ", ";
	     PP.break ppstrm {nsp=1,offset=0};
	     pps (Int.toString index);
	     pps ")";
	     closeBox())
	  | ppTycI (LK.TC_SUM(tycs)) =
	    (pps "TC_SUM(";
	     PP.break ppstrm {nsp=1,offset=1};
	     ppList' {sep=", ", pp=ppTyc'} tycs;
	     pps ")")
	    (* TC_FIX is a recursive datatype constructor 
	       from a (mutually-)recursive family *)
	  | ppTycI (LK.TC_FIX((numStamps, datatypeFamily, freetycs), index)) =
	    (openHOVBox 1;
	     pps "TC_FIX(";
	     PP.break ppstrm {nsp=1,offset=1};
	     (case (LK.tc_out datatypeFamily) of
		  LK.TC_FN(params, rectyc) => (* generator function *) 
		  let fun ppMus 0 = ()
			| ppMus i = (pps "mu"; 
				     ppi ppstrm i; 
				     pps " "; 
				     ppMus (i - 1))
		  in 
		  (pps "RECTYCGEN(";
		   if (length params) > 0 then (pps "[";
						ppMus (length params);
						pps "]")
		   else ();
		   PP.break ppstrm {nsp=1,offset=1};  
		  (case (LK.tc_out rectyc) of
			 (rectycI as LK.TC_FN _) => ppTycI rectycI
		       | LK.TC_SEQ(dconstycs) => 
			 ppTyc' (List.nth(dconstycs, index))
		       | tycI => ppTycI tycI);
		  PP.break ppstrm {nsp=0,offset=0};
		  pps ")")
		  end
		| _ => pps "<No rectyc generator>");
	     PP.break ppstrm {nsp=0,offset=0};
	     pps ")";
	     closeBox()
	 (*    pps "TC_FIX(";
	     PP.break ppstrm {nsp=1,offset=1};
	     pps "nStamps = ";
	     pps (Int.toString numStamps);
	     pps ",";
	     PP.break ppstrm {nsp=1, offset=0};
	     pps "datatypeFamily = ";
	     ppTyc' datatypeFamily;
	     pps ", ";
	     PP.break ppstrm {nsp=1, offset=0};
	     pps "freeTycs = ";
	     ppList' {sep = ", ", pp = ppTyc'} freetycs;
	     pps ", ";
	     PP.break ppstrm {nsp=1, offset=0};
	     pps "index = ";
	     pps (Int.toString index);
	     pps ")";
	     closeBox() *) )
	  | ppTycI (LK.TC_ABS tyc) =
	    (pps "TC_ABS(";
	     PP.break ppstrm {nsp=1,offset=1};
	     ppTyc' tyc;
	     pps ")")
	  | ppTycI (LK.TC_BOX tyc) =
	    (pps "TC_BOX(";
	     PP.break ppstrm {nsp=1,offset=1};
	     ppTyc' tyc;
	     pps ")")
	    (* rflag is a tuple kind template, a singleton datatype RF_TMP *)
	  | ppTycI (LK.TC_TUPLE (rflag, tycs)) =
	    (pps "TC_TUPLE(";
	     PP.break ppstrm {nsp=1,offset=1};
	     ppList' {sep="* ", pp=ppTyc'} tycs;
	     pps ")")
	    (* fflag records the calling convention: either FF_FIXED or FF_VAR *)
	  | ppTycI (LK.TC_ARROW (fflag, argTycs, resTycs)) =
	    (pps "TC_ARROW(";
	     PP.break ppstrm {nsp=1,offset=1};
	     (case fflag of LK.FF_FIXED => pps "FF_FIXED"
			  | LK.FF_VAR(b1, b2) => (pps "<FF_VAR>" (*;
						   ppBool b1;
						  pps ", ";
						  ppBool b2; 
						  pps ")"*) ));
	     ppList' {sep="* ", pp=ppTyc'} argTycs;
	     pps ", ";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppList' {sep="* ", pp=ppTyc'} resTycs;
	     pps ")")
	    (* According to ltykernel.sml comment, this arrow tyc is not used *)
	  | ppTycI (LK.TC_PARROW (argTyc, resTyc)) =
	    (pps "TC_PARROW(";
	     PP.break ppstrm {nsp=1,offset=1};
	     ppTyc' argTyc;
	     pps ", ";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppTyc' resTyc;
	     pps ")")
	  | ppTycI (LK.TC_TOKEN (tok, tyc)) =
	    (pps "TC_TOKEN(";
	     PP.break ppstrm {nsp=1,offset=1};
	     pps (LK.token_name tok);
	     pps ", ";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppTyc' tyc;
	     pps ")")
	  | ppTycI (LK.TC_CONT tycs) = 
	    (pps "TC_CONT(";
	     PP.break ppstrm {nsp=1,offset=1};
	     ppList' {sep=", ", pp=ppTyc'} tycs;
	     pps ")")
	  | ppTycI (LK.TC_IND (tyc, tycI)) =
	    (openHOVBox 1;
	     pps "TC_IND(";
	     PP.break ppstrm {nsp=1,offset=1};
	     ppTyc' tyc;
	     pps ", ";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppTycI tycI;
	     pps ")";
	     closeBox())
	  | ppTycI (LK.TC_ENV (tyc, ol, nl, tenv)) =
	    (openHOVBox 1;
	     pps "TC_ENV(";
	     PP.break ppstrm {nsp=1,offset=1};
	     ppTyc' tyc;
	     pps ", ";
	     PP.break ppstrm {nsp=1,offset=0};
	     pps "ol = ";
	     pps (Int.toString ol);
	     pps ", ";
	     pps "nl = ";
	     pps (Int.toString nl);
	     pps ", ";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppList' {sep=", ", pp=(ppTycEnvElem ppstrm)} (tycEnvFlatten tenv);
	     closeBox())
    in ppTycI (LK.tc_out tycon)
    end (* ppTyc *)

fun ppTycEnv ppstrm (tycEnv : LK.tycEnv) =
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
    in
	openHOVBox 1;
	pps "TycEnv(";
	ppList ppstrm {sep=", ", pp=ppTycEnvElem ppstrm} (tycEnvFlatten tycEnv);
	pps ")";
	closeBox()
    end (* function ppTycEnv *)

end (* local *)	    
	     
end
