(* mccommon.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* TODO: this module requires a signature ! *)

structure MCCommon =
struct

local structure EM = ErrorMsg
      structure DA = Access
      structure LV = LambdaVar
      structure T = Types
      structure R = Rules
      open VarCon PLambda Absyn
      (* also used: IntConst, ListPair *)
in

type ruleno = R.ruleno  (* == int, the index number of a rule in the match, zero-based *)
type ruleset = R.ruleset
   (* a set of rule numbers, maintained in strictly ascending order without duplicates *)
   (* == IntBinarySet.set *)

type binding = var * ruleno
   (* a variable bound at some point in the given rule, either as a
    * basic var pattern (VARpat) or through an "as" pattern (LAYEREDpat) *)
type varBindings = binding list  (* variables bound by VARpat *)		    
type asBindings = binding list   (* variables bound by LAYEREDpat, i.e. an "as" pattern *)
			  
type dataCon  (* datatype constructors in patterns *)
  = datacon * tyvar list

datatype constCon
  = INTconst of int IntConst.t
  | WORDconst of int IntConst.t
  | STRINGconst of string
  | CHARconst of char  (* not needed? -- chars covered by int? *)
	     
(* and-or trees.
   -- There is one form of AND node, implying matching of all subnodes,
   -- There are three forms of OR nodes: DATA, CONST, VEC, descriminating
      on datatype datacons, primitive constants (int, word, char?, and string),
      or vector lengths, respectively.
   -- There are two/three kinds of terminal nodes: LEAF, [VAR], and CONST, that
      have no pattern structure below them.  CONST nodes have branches, but
      the branches have no andor components.
*)
datatype andor
  = AND of andor list
	(* record, tuple, or _implicit_ tuple of constructor arguments,
	 * or _implicit_ tuple of elements of a vector *)
  | DATA of dataBranch list  (* invariant: |dataBranch list| >= 1 *)
  | CONST of constBranch list  (* invariant: |constBranch list| >= 1 *)
  | VEC of vecBranch list  (* invariant: |vecBranch list| >= 1 *)
  | VAR  (* of binding (* terminal var node, with no subsidiary structure *) *)
  | AS of andor * asBindings
     (* "as" bound variables. Match the andor _and_ bind the appropriate var from as bindings *)
  | VARS of andor * varBindings
     (* match the andor, _or_ any of the default vars in varBindings
      * representing var patterns at this position in the pattern space. *)
			    
 withtype dataBranch = dataCon * andor option * R.ruleset
         (* The andor option is the andor tree for the argument pattern(s)
          * for dataCon, if any (i.e. NONE if dataCon is a constant). *)
     and vecBranch = int * andor * R.ruleset
         (* andor is an AND. vecBranches are ordered by the int (length) field *)
     and constBranch = constCon * R.ruleset
         (* no subsidiary (argument) patterns, so no andor  *)

(* constantEq: constCon * constCon -> bool
 * constantEq(p1,p2) iff p1 and p2 are equal pattern constants *)
fun constantEq (DATApcon (d1, _), DATApcon (d2, _)) = conEq(d1, d2)
  | constantEq (INTconst n, INTconst n') = IntConst.same(n,n')
  | constantEq (WORDconst n, WORDconst n') = IntConst.same(n,n')
  | constantEq (STRINGconst s, STRINGconst s') = s = s'
  | constantEq _ = false


(* ================================================================================ *)
(* decision trees *)

datatype path
  = RECORDPATH of path list
  | PIPATH of int * path
  | VLENPATH of ty * path       (* vector *)
  | VPIPATH of int * ty * path  (* vector + index? *)
  | DELTAPATH of pcon * path
  | ROOTPATH
	
(* decision: decision "trees" *)
(* ABSCONDEC produced only by flattenConstraints *)
datatype decision
  = CASEDEC of
     {path : path,
      sign : DA.consig,
      cases : dec_case list,
      defaults: ruleset}
  | BINDDEC of
     {path : path,
      rules : ruleset}
withtype dec_case = pcon * ruleset * decision list

(* decision tree *)
datatype dectree
  = CASETEST of
     {path : path,
      sign : DA.consig,
      caseTrees : (pcon * dectree) list,
      default : dectree option}
  | BIND of
     {path : path,
      dectree : dectree}
  | RHS of int


fun bug s = EM.impossible ("MCCommon: " ^ s)

(* mkRECORDpat : Absyn.pat -> Absyn.pat list -> Absyn.pat *)
(* replace field patterns in a record pat (1st arg) with elements of 2nd arg;
 * used only in MatchComp$orExpand *)
fun mkRECORDpat (RECORDpat{fields, flex=false, typ, ...}) pats =
      RECORDpat {flex=false, typ=typ,
                 fields=ListPair.map(fn((id,_),p)=>(id,p))(fields,pats)}
  | mkRECORDpat (RECORDpat{flex=true,...}) _ =
      bug "mkRECORDpat - flex record"
  | mkRECORDpat _ _ = bug "mkRECORDpat - non-record"

(* based on the fact that no two datacons of a datatype have the same rep value,
 * and conrep is an equality type. This function should be in TypesUtil. *)
fun eqDcon(DATACON{rep=a1,...}: datacon, DATACON{rep=a2,...}: datacon) = (a1 = a2)

fun pathEq(PIPATH(i1,p1),PIPATH(i2,p2)) = i1=i2 andalso pathEq(p1,p2)
  | pathEq(VPIPATH(i1,_,p1),VPIPATH(i2,_,p2)) = i1=i2 andalso pathEq(p1,p2)
  | pathEq(VLENPATH(_, p1),VLENPATH(_, p2)) = pathEq(p1,p2)
  | pathEq(DELTAPATH(c1,p1),DELTAPATH(c2,p2)) =
	               constantEq(c1,c2) andalso pathEq(p1,p2)
  | pathEq(ROOTPATH,ROOTPATH) = true
  | pathEq _ = false

(* lvarEnv: association list mapping paths to lvars *)
type lvarEnv = (path * LV.lvar) list
fun lookupPath (a: path, (b,c)::d : lvarEnv) : LV.lvar =
       if pathEq(a,b) then c else lookupPath(a, d)
  | lookupPath _ = bug "lookupPath nil 2nd arg"

(* these functions should be in TypesUtil, along with eqDcon *) 
fun signOfDatacon (DATACON{sign,...}) = sign
fun constOfDatacon (DATACON{const,...}) = const

end (* toplevel local *)
end (* structure MCCommon *)
