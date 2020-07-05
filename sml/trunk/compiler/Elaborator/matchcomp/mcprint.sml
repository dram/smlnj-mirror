(* FLINT/matchcomp/mcprint.sml *)

(* Printing the structures of the new match compiler: andor, decTree, mcexp *)

structure MCPrint =
struct

local
    structure S = Symbol
    structure PP = PrettyPrint
    structure PU = PPUtil
    structure T = Types
    structure TU = TypesUtil
    structure V = Var
    structure SV = SVar
    structure R = Rules
    open Absyn MCTypes
in

fun CC f x y = f y x

fun numlabel i = S.make (Int.toString i)

fun checkpat (n,nil) = true
  | checkpat (n, (sym,_)::fields) =
    S.eq(sym, numlabel n) andalso checkpat(n+1,fields)

fun isTUPLEpat (RECORDpat{fields=[_]}) = false  (* one element records are not tuples *)
  | isTUPLEpat (RECORDpat{fields}) = checkpat(1,fields)
  | isTUPLEpat _ = false

(* pretty printing (restricted) patterns *)
fun ppVar ppstrm (var: V.var) =
    PP.string ppstrm (S.name(V.varName var))
	      
fun ppDcon ppstrm (dcon: T.datacon) =
    PP.string ppstrm (S.name(TU.dataconName dcon))

fun ppPat ppstrm =
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = PU.en_pp ppstrm
	fun ppPat' (_,0) = pps "<pat>"
	  | ppPat' (VARpat v,_) = ppVar ppstrm v
	  | ppPat' (WILDpat,_) = pps "_"
          | ppPat' (NUMpat(src, _), _) = pps src
	  | ppPat' (STRINGpat s,_) = PU.ppString ppstrm s
	  | ppPat' (CHARpat s,_) = (pps "#"; PU.ppString ppstrm s)
	  | ppPat' (LAYEREDpat (v,p),d) =
	      (openHVBox 0;
	       ppVar ppstrm v; pps " as "; ppPat'(p,d-1);
	       closeBox ())
		    (* Handle 0 length case specially to avoid {,...}: *)
	  | ppPat' (RECORDpat{fields=[]},_) = pps "()"
	  | ppPat' (r as RECORDpat{fields},d) =
	      if isTUPLEpat r
	      then PU.ppClosedSequence ppstrm
		     {front=(CC PP.string "("),
		      sep=(fn ppstrm => (PP.string ppstrm ",";
					 PP.break ppstrm {nsp=0,offset=0})),
		      back=(CC PP.string ")"),
		      pr=(fn _ => fn (sym,pat) => ppPat'(pat,d-1)),
		      style=PU.INCONSISTENT}
		     fields
	      else PU.ppClosedSequence ppstrm
		     {front=(CC PP.string "{"),
		      sep=(fn ppstrm => (PP.string ppstrm ",";
					 PP.break ppstrm {nsp=0,offset=0})),
		      back=(fn ppstrm => PP.string ppstrm "}"),
		      pr=(fn ppstrm => fn (sym,pat) =>
			  (PU.ppSym ppstrm sym; PP.string ppstrm "=";
			   ppPat'(pat,d-1))),
		      style=PU.INCONSISTENT}
		     fields
	  | ppPat' (VECTORpat(nil,_), d) = pps "#[]"
	  | ppPat' (VECTORpat(pats,_), d) =
	      let fun pr _ pat = ppPat'(pat, d-1)
	       in PU.ppClosedSequence ppstrm
		    {front=(CC PP.string "#["),
		     sep=(fn ppstrm => (PP.string ppstrm ",";
					PP.break ppstrm {nsp=0,offset=0})),
		     back=(CC PP.string "]"),
		     pr=pr,
		     style=PU.INCONSISTENT}
		    pats
	      end
	  | ppPat' (pat as (ORpat _), d) = let
	      fun mkList (ORpat(hd, tl)) = hd :: mkList tl
		| mkList p = [p]
	      fun pr _ pat = ppPat'(pat, d-1)
	      in
		PU.ppClosedSequence ppstrm {
		    front = (CC PP.string "("),
		    sep = fn ppstrm => (PP.break ppstrm {nsp=1,offset=0};
                                        PP.string ppstrm "| "),
		    back = (CC PP.string ")"),
		    pr = pr,
		    style = PU.INCONSISTENT
		  } (mkList pat)
	      end
	  | ppPat' (CONpat(e,_),_) = ppDcon ppstrm e
	  | ppPat' (p as APPpat _, d) =
	      ppDconPat ppstrm (p,d)
	  | ppPat' _ = bug "ppPat'"
     in ppPat'
    end

and ppDconPat ppstrm =
    let val {openHOVBox, openHVBox, closeBox, pps, ppi, ...} = PU.en_pp ppstrm
	fun ppDconPat'(_,0) = pps "<pat>"
	  | ppDconPat'(CONpat(dcon,_),_) =
	      PU.ppSym ppstrm (TU.dataconName dcon)
	  | ppDconPat'(LAYEREDpat(v,base),d) =
	     (openHOVBox 0;
	      pps "("; ppVar ppstrm v; PP.break ppstrm {nsp=1,offset=2};
	      pps " as "; ppPat ppstrm (base,d-1); pps ")";
	      closeBox ())
	  | ppDconPat'(APPpat(dcon,_,p),d) =
	      let val dname = TU.dataconName dcon
	       in openHOVBox 2;
		  pps "(";
		  pps dname; PP.break ppstrm {nsp=1,offset=0};
		  ppDconPat'(p,d-1);
		  pps ")";
		  closeBox ()
	      end
	  | ppDconPat' (p,d) = ppPat ppstrm (p,d)
     in ppDconPat'
    end

(*
(* keys and paths *)

fun keyToString (D(dcon,_)) = substring((TU.dataconName dcon),0,1)
  | keyToString (V(n,_)) = "V"^(Int.toString n)
  | keyToString (I{ival,ty}) = (IntInf.toString ival)
  | keyToString (W{ival,ty}) = (IntInf.toString ival)
  | keyToString (C c) = "C"^(Char.toString c)
  | keyToString (S s) = "S["^ s ^ "]"
  | keyToString (R i) = Int.toString i

fun pathToString path =
    concat(map keyToString path)
*)

fun ppPath ppstrm path =
    (PP.openHBox ppstrm;
     PP.string ppstrm "["; PP.string ppstrm (pathToString path); PP.string ppstrm "]";
     PP.closeBox ppstrm)

(* rulesets *)
	
fun ppRuleset ppstrm rules =
    let val rules' = R.listItems rules
    in PP.openHBox ppstrm;
       PU.pps ppstrm "{";
       PU.ppSequence ppstrm
	 {sep = (fn ppstrm => PU.pps ppstrm ","),
	  pr = (fn ppstrm => fn r => PU.ppi ppstrm r),
	  style = PU.INCONSISTENT}
	 rules';
       PU.pps ppstrm "}";
       PP.closeBox ppstrm
    end

fun ppVarBindings ppstrm vars =	
    let fun ppvar ppstrm (v,r) =
            (PP.openHBox ppstrm;
	     PP.string ppstrm "(";
	     PP.string ppstrm (S.name(V.varName v));
	     PP.string ppstrm ",";
	     PU.ppi ppstrm r;
	     PP.string ppstrm ")";
	     PP.closeBox ppstrm)
    in PU.ppSequence ppstrm
	   {sep = (fn ppstrm => PP.break ppstrm {nsp=1,offset=0}),
	    pr = ppvar,
	    style = PU.INCONSISTENT}
	   vars
    end

(* andor trees *)
	
(* ppAndor : ppstrm -> andor -> unit *)
(* bare-bones pretty printer for AND-OR nodes *)
fun ppAndor ppstrm =
    let fun ppNode ppstrm (AND{path,direct,defaults,children,...}) =
	    (PP.openHOVBox ppstrm (PP.Abs 0);
	     PP.openHBox ppstrm;
	     ppPath ppstrm path;
	     PP.break ppstrm {nsp=1,offset=0};
             PP.string ppstrm "AND";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppRuleset ppstrm direct;
	     PP.break ppstrm {nsp=1,offset=0};
	     ppRuleset ppstrm defaults;
	     PP.closeBox ppstrm;
	     ppAndChildren ppstrm children;
	     PP.closeBox ppstrm)
	  | ppNode ppstrm (OR{path,direct,defaults,variants,...}) = 
	    (PP.openHOVBox ppstrm (PP.Abs 0);
             PP.openHBox ppstrm;
	     ppPath ppstrm path;
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "OR";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppRuleset ppstrm direct;
	     PP.break ppstrm {nsp=1,offset=0};
	     ppRuleset ppstrm defaults;
	     PP.closeBox ppstrm;
	     ppVariants ppstrm variants;
	     PP.closeBox ppstrm)
	  | ppNode ppstrm (SINGLE _) = PP.string ppstrm "SINGLE"
	  | ppNode ppstrm (andor as VARS{vars,defaults,...}) =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm "VARS";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppVarBindings ppstrm vars;
	     PP.break ppstrm {nsp=1,offset=0};
	     ppRuleset ppstrm defaults;
	     PP.closeBox ppstrm)
	  | ppNode ppstrm (LEAF{direct,defaults,...}) =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm "LEAF";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppRuleset ppstrm direct;
	     PP.break ppstrm {nsp=1,offset=0};
	     ppRuleset ppstrm defaults;
	     PP.closeBox ppstrm)
	  | ppNode ppstrm (INITIAL) = PP.string ppstrm "INITIAL"
	and ppAndChildren ppstrm nodes =
	    (PP.openVBox ppstrm (PP.Abs 3);
	     (* PP.cut ppstrm; *)
	     PU.ppvseq ppstrm 0 "" ppNode nodes;
	     PP.closeBox ppstrm)
	and ppVariants ppstrm variants =
	    (PP.openVBox ppstrm (PP.Abs 3);
	     (* PP.cut ppstrm; *)
	     PU.ppvseq ppstrm 0 "" ppVariant variants;
	     PP.closeBox ppstrm)
	and ppVariant ppstrm (key, node) =
	    (PP.openHOVBox ppstrm (PP.Abs 0);
	     PP.string ppstrm (keyToString key);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppNode ppstrm node;
	     PP.closeBox ppstrm)
    in ppNode ppstrm
    end


(* decTree printing *)

(* ppDecTree : ppstrm -> decTree -> unit *)
val ppDecTree =
    let fun ppDec ppstrm (CHOICE{node,choices,default}) =
            (PP.openHBox ppstrm;
	     PP.string ppstrm "DEC";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppPath ppstrm (getPath node);
	     ppChoices ppstrm (choices,default);
	     PP.closeBox ppstrm)
	  | ppDec ppstrm (DLEAF r) =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm "DLEAF";
	     PP.break ppstrm {nsp=1,offset=0};
	     PU.ppi ppstrm r;
	     PP.closeBox ppstrm)
	  | ppDec ppstrm RAISEMATCH =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm "MATCH";
	     PP.closeBox ppstrm)
	and ppChoices ppstrm (decvariants,default) =
            (PP.openVBox ppstrm (PP.Abs 2);
	     PU.ppvseq ppstrm 0 "" ppDecvariant decvariants;
	     (case default
	        of SOME dectree =>
          	     (PP.cut ppstrm;
		      PP.openHOVBox ppstrm (PP.Abs 0);
	              PP.string ppstrm "*";
		      PP.break ppstrm {nsp=1,offset=0};
		      ppDec ppstrm dectree;
		      PP.closeBox ppstrm)
		 | NONE => ());
	     PP.closeBox ppstrm)
	and ppDecvariant ppstrm (key, decTree) =
	    (PP.openHOVBox ppstrm (PP.Abs 0);
	     PP.string ppstrm (keyToString key);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppDec ppstrm decTree;
	     PP.closeBox ppstrm)
    in ppDec
    end


(* mcexp printing *)

fun ppSvar ppstrm svar = PU.ppSym ppstrm (SV.svarName svar)

fun ppSvars ppstrm svars =
    (PP.openHBox ppstrm;
     PP.string ppstrm "(";
     PU.ppSequence ppstrm
	 {sep = (fn ppstrm => PP.string ppstrm ","),
	  pr = ppSvar,
	  style = PU.INCONSISTENT}
	 svars;
     PP.string ppstrm ")";
     PP.closeBox ppstrm)
	
fun ppVar ppstrm (var: Var.var) = PU.ppSym ppstrm (V.varName var)

fun ppVars ppstrm svars =
    (PP.openHBox ppstrm;
     PP.string ppstrm "(";
     PU.ppSequence ppstrm
	 {sep = (fn ppstrm => PP.string ppstrm ","),
	  pr = ppVar,
	  style = PU.INCONSISTENT}
	 svars;
     PP.string ppstrm ")";
     PP.closeBox ppstrm)

val ppCode =
    let fun ppc ppstrm (Letr(v,vars,body)) =
	    (PP.openVBox ppstrm (PP.Abs 2);
             PP.openHBox ppstrm;
	     PP.string ppstrm "Letr";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppSvars ppstrm vars;
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "=";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppSvar ppstrm v;
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "in";
	     PP.closeBox ppstrm;
	     PP.cut ppstrm;
	     ppc ppstrm body;
	     PP.closeBox ppstrm)
	  | ppc ppstrm (Case(v,cases,default)) =
	    (PP.openVBox ppstrm (PP.Rel 2);
	     PP.openHBox ppstrm;
	     PP.string ppstrm "Case";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppSvar ppstrm v;
	     PP.closeBox ppstrm;
	     ppcases ppstrm (cases,default);
	     PP.closeBox ppstrm)
	  | ppc ppstrm (Var svar) =
	     ppSvar ppstrm svar
	  | ppc ppstrm (Letf(funsvar,funexp,body)) = 
	    (PP.openHBox ppstrm;
	     PP.string ppstrm "Letf ";
	     ppSvar ppstrm funsvar;
     	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "=";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppc ppstrm funexp;
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "in";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppc ppstrm body;
	     PP.closeBox ppstrm)
	  | ppc ppstrm (Letm(vars, svars, body)) =
    	    (PP.openVBox ppstrm (PP.Abs 2);
             PP.openHBox ppstrm;
	     PP.string ppstrm "Letm";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppVars ppstrm vars;
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "=";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppSvars ppstrm svars;
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "in";
	     PP.closeBox ppstrm;
	     PP.cut ppstrm;
	     PP.string ppstrm "<RHS:absyn>";
	     PP.closeBox ppstrm)
	  | ppc ppstrm (Sfun(vars, body)) =
    	    (PP.openVBox ppstrm (PP.Abs 2);
             PP.openHBox ppstrm;
	     PP.string ppstrm "sfn";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppVars ppstrm vars;
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "=>";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "<RHS:absyn>";
	     PP.closeBox ppstrm)
	  | ppc ppstrm (Sapp(funsvar, argsvars)) = 
	    (PP.openHBox ppstrm;
	     ppSvar ppstrm funsvar;
	     ppSvars ppstrm argsvars;
	     PP.closeBox ppstrm)
	  | ppc ppstrm MATCH =
	     PP.string ppstrm "MATCH"
	and ppcases ppstrm (cases,default) =
	    let fun prElems [el] = ppcase ppstrm el
		  | prElems (el::rest) =
		      (ppcase ppstrm el; 
		       PP.cut ppstrm;
                       prElems rest)
		  | prElems [] = ()
	    in PP.openVBox ppstrm (PP.Abs 0);
	       PP.cut ppstrm;
               prElems cases;
	       (case default
		 of SOME exp =>
		    (PP.cut ppstrm;
		     PP.string ppstrm "* => ";
		     ppc ppstrm exp)
		  | NONE => ());
               PP.closeBox ppstrm
	    end
	and ppcase ppstrm (key,svarOp,rhsExp) =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm (keyToString key);
	     PP.break ppstrm {nsp=1,offset=0};
	     (case svarOp
	       of NONE => ()
		| SOME svar =>
		  (ppSvar ppstrm svar;
		   PP.break ppstrm {nsp=1,offset=0}));
	     PP.string ppstrm "=>";
	     PP.break ppstrm {nsp=1,offset=0};
	     ppc ppstrm rhsExp;
	     PP.closeBox ppstrm)
    in ppc
    end
		   
(* top-level printing functions *)

val printDepth = ref 10
		     
(* top-level print functions *)
fun tppPat pat =
    PP.with_default_pp(fn ppstrm => ppPat ppstrm (pat,!printDepth))

fun tppPats pats =
    List.app tppPat pats
	     
fun tppAndor andor =
    PP.with_default_pp(fn ppstrm => ppAndor ppstrm andor)

fun tppDecTree dectree =
    PP.with_default_pp(fn ppstrm => ppDecTree ppstrm dectree)

fun tppCode mcexp =
    PP.with_default_pp(fn ppstrm => ppCode ppstrm mcexp)
		      
fun tppRules ruleset =
    PP.with_default_pp(fn ppstrm => ppRuleset ppstrm ruleset)

fun tppPath path =
    PP.with_default_pp(fn ppstrm => ppPath ppstrm path)

end (* local *)
end (* structure MCPrint *)
