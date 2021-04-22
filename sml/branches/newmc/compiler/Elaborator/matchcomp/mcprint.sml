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
    structure A = Access
    structure LV = LambdaVar
    structure V = VarCon
    structure SV = SVar
    structure R = Rules
    structure LS = Layers.Set
    structure LL = LiveLayers
    structure K = Key
    open Absyn MCTypes
in

fun CC f x y = f y x

fun ppPath ppstrm path =
    PP.string ppstrm (pathToString path);

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

fun ppLayerset ppstrm layers =
    let val layers' = LS.listItems layers
    in PP.openHBox ppstrm;
       PU.pps ppstrm "{";
       PU.ppSequence ppstrm
	 {sep = (fn ppstrm => PU.pps ppstrm ","),
	  pr = (fn ppstrm => fn l => PU.pps ppstrm (Layers.layerToString l)),
	  style = PU.INCONSISTENT}
	 layers';
       PU.pps ppstrm "}";
       PP.closeBox ppstrm
    end

fun ppVarBindings ppstrm varbindings =
    let fun ppvar ppstrm (var,layer) =
            (PP.openHBox ppstrm;
	     PP.string ppstrm "(";
	     PP.string ppstrm (S.name(V.varName var));
	     PP.string ppstrm ",";
	     PU.pps ppstrm (Layers.layerToString layer);
	     PP.string ppstrm ")";
	     PP.closeBox ppstrm)
    in PU.ppSequence ppstrm
	   {sep = (fn ppstrm => PP.break ppstrm {nsp=1,offset=0}),
	    pr = ppvar,
	    style = PU.INCONSISTENT}
	   varbindings
    end

fun ppLvar ppstrm (var : V.var) =
    (case var
      of V.VALvar{access = A.LVAR lv,...} =>
	   PP.string ppstrm ("v" ^ (LV.prLvar lv))
       | _ => bug "ppLvar")

(* andor trees *)
		   
(* ppAndor : ppstrm -> andor -> unit *)
(* bare-bones pretty printer for AND-OR nodes *)
fun ppAndor ppstrm =
    let fun ppNode ppstrm (AND{info={id,path,...}, live, children, ...}) =
	    (PP.openHOVBox ppstrm (PP.Abs 0);
	     PP.openHBox ppstrm;
	     ppPath ppstrm path;
	     PP.break ppstrm {nsp=1,offset=0};
             PP.string ppstrm "AND";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm (Int.toString id);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppLayerset ppstrm (LL.directs live);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppLayerset ppstrm (LL.defaults live);
	     PP.closeBox ppstrm;
	     ppAndChildren ppstrm children;
	     PP.closeBox ppstrm)
	  | ppNode ppstrm (OR{info={id, path,...}, live, variants, ...}) =
	    (PP.openHOVBox ppstrm (PP.Abs 0);
             PP.openHBox ppstrm;
	     ppPath ppstrm path;
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "OR";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm (Int.toString id);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppLayerset ppstrm (LL.directs live);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppLayerset ppstrm (LL.defaults live);
	     PP.closeBox ppstrm;
	     ppVariants ppstrm variants;
	     PP.closeBox ppstrm)
	  | ppNode ppstrm (SINGLE{info = {id,path,...}, variant = (key,arg), ...}) =
	    (PP.openHOVBox ppstrm (PP.Abs 0);
             PP.openHBox ppstrm;
	     ppPath ppstrm path;
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "SINGLE";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm (Int.toString id);
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.closeBox ppstrm;
	     PP.openVBox ppstrm (PP.Abs 2);
	     PP.cut ppstrm;
	     ppVariant ppstrm (key, arg);
	     PP.closeBox ppstrm;
	     PP.closeBox ppstrm)
	  | ppNode ppstrm (VARS{info = {id,path,vars,...}, layers, ...}) =
	    (PP.openHBox ppstrm;
	     ppPath ppstrm path;
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm "VARS";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm (Int.toString id);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppVarBindings ppstrm vars;
	     PP.break ppstrm {nsp=1,offset=0};
	     ppLayerset ppstrm layers;
	     PP.closeBox ppstrm)
	  | ppNode ppstrm (LEAF{info = {id, ...}, live,  ...}) =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm "LEAF";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm (Int.toString id);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppLayerset ppstrm (LL.directs live);
	     PP.break ppstrm {nsp=1,offset=0};
	     ppLayerset ppstrm (LL.defaults live);
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
	     PU.ppvseq ppstrm 0 "" ppVariant (Variants.listItems' variants);
	     PP.closeBox ppstrm)
	and ppVariant ppstrm (key, node) =
	    (PP.openHBox ppstrm (* (PP.Abs 0) *);
	     PP.string ppstrm (K.keyToString key);
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
	     PP.string ppstrm "CHOICE";
	     PP.break ppstrm {nsp=1,offset=0};
	     PP.string ppstrm (Int.toString (getId node));
	     PP.break ppstrm {nsp=1,offset=0};
	     ppPath ppstrm (getPath node);
	     ppChoices ppstrm (choices,default);
	     PP.closeBox ppstrm)
	  | ppDec ppstrm (DLEAF (layer, _)) =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm "DLEAF";
	     PP.break ppstrm {nsp=1,offset=0};
	     PU.pps ppstrm (Layers.layerToString layer);
	     PP.closeBox ppstrm)
	  | ppDec ppstrm (DMATCH _) =
	    (PP.openHBox ppstrm;
	     PP.string ppstrm "MATCH";
	     PP.closeBox ppstrm)
	and ppChoices ppstrm (decvariants,default) =
            (PP.openVBox ppstrm (PP.Abs 3);
	     PU.ppvseq ppstrm 0 "" ppDecvariant (Variants.listItems' decvariants);
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
	    (PP.openHBox ppstrm;
	     PP.string ppstrm (K.keyToString key);
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

(* top-level printing functions *)

val printDepth = ref 10

(* top-level print functions *)

fun tppAndor andor =
    PP.with_default_pp(fn ppstrm => ppAndor ppstrm andor)

fun tppDecTree dectree =
    PP.with_default_pp(fn ppstrm => ppDecTree ppstrm dectree)

fun tppRules ruleset =
    PP.with_default_pp(fn ppstrm => ppRuleset ppstrm ruleset)

fun tppLayers layerset =
    PP.with_default_pp(fn ppstrm => ppLayerset ppstrm layerset)

fun tppPath path =
    PP.with_default_pp(fn ppstrm => ppPath ppstrm path)

end (* local *)
end (* structure MCPrint *)
