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

fun bug s = EM.impossible ("MCCommon: " ^ s)

type ruleno = R.ruleno  (* == int, the index number of a rule in the match, zero-based *)
type ruleset = R.ruleset
   (* a set of rule numbers, maintained in strictly ascending order without duplicates *)
   (* == IntBinarySet.set *)

type binding = var * ruleno
   (* a variable bound at some point in the given rule, either as a
    * basic var pattern (VARpat) or through an "as" pattern (LAYEREDpat) *)
type varBindings = binding list  (* variables bound by VARpat *)		    
type asBindings = binding list   (* variables bound by LAYEREDpat, i.e. an "as" pattern *)
			  
(* datatype constructors in patterns *)
type dataCon = datacon * tyvar list   (* do we need the tyvar list (bound tyvars)? *)

datatype constCon
  = INTconst of int IntConst.t
  | WORDconst of int IntConst.t
  | STRINGconst of string
  | CHARconst of char  (* not needed? -- chars treated as int (see charCon in andor.sml) *)
	     

(* paths: 
   paths locate points in the "pattern space" determined by a sequence of patterns
      (a subtree of the complete pattern tree determined by the common type of
      the patterns). Points in the pattern space correspond to andor nodes, which
      therefore have a unique identifying path.
*)

(* Path constructor name changes (from the 110.97 version):
   RECORDPATH not used, eliminated
   VLENPATH replaced by VL
   PIPATH ==> RL   -- record component selection
   VPIPATH ==> eliminated -- vector element selection (use RL)
   DELTAPATH ==> DL, CL, VL
   CHOICEC not used?  No subsidary nodes beneath a constant pattern
*)

datatype link
  = RL  of int       (* select ith component of a product *)
  | DL  of dataCon   (* choosing a branch of an datatype node on datacon *)
  | CL  of constCon  (* choosing a branch of an constant node on const. Not used? *)
  | VL of int        (* choosing a branch of an vector node with vector length i *)

type rpath = link list (* links are in reverse order from node to root *)
type path = link list (* links ordered from root to node *)

fun linkEq (RL i1, RL i2) = i1 = i2
  | linkEq (VL i1, VL i2) = i1 = i2
  | linkEq (DL c1, DL c2) = TU.eqDatacon(c1,c2)
  | linkEq (CL c1, CL c2) = constConEq
  | linkEq _ = false

fun pathEq(link1::rest1, link2::rest2) = linkEq(link1,link2) andalso pathEq(rest1,rest2)
  | pathEq(nil,nil) = true
  | pathEq _ = false

val pathEq = ListPair.allEq linkEq

(* AND-OR trees.
   AND-OR trees (type andor) have a node for each point in the pattern space
   and each node captures the combinded information of the (sub) patterns at
   that point in the pattern space, which is created by "merging" the patterns
   (functions initAndor and mergeAndor).

   -- There is one form of AND node, implying matching of all subnodes, used
      for records, tuples, and the elements of a vector,
   -- There are three forms of OR or "choice" nodes: ORdata, ORvec, and ORconst,
      descriminating among datatype datacons of a given datatype, vector lengths,
      or primitive constants (int, word, char?, and string).
   -- There is one kind of terminal node:  VAR. This is an atomic pattern,
      but may be merged with other, structured, patterns, in which case it is
      recorded under defaults (vars) in the merged node. ORconst nodes have children, but
      the children are teminal "nodes". So are dataChild links associated
      with constant data constructors (e.g. nil).
   -- rulesets: there rulesets that indicate which rules are "live" (matchable) at
      a given node. These are (currently) stored only for the children of OR nodes,
      which is were they change.  The live rules for an AND node propagage without
      change to its children -- there is no rule filtering effect.
   -- at the root node, the live ruleset is all rules. As one traverses a branch,
      the live ruleset is restricted by droping rules that are incompatible with
      a branch choice (at an OR child). But a variable at a node introduces a 
      default rule that propagates downward to all descendent nodes.
   -- How to account for (keep track of) the "default" rules introduced by VARS
      bindings at the current OR node or at an AND node. defaults trickle down
      to descendent nodes.
   -- AS bindings indicate a layered variable binding at a node, but do not affect
      the live ruleset at that node or its descendents.
   -- A VAR node is absorbed if it is merged with an AND or OR node. It it is 
      merged with another VAR node, then it accumulates the bound variables (and
      their rules).  I.e. the defaults and asvars are the unions of the two VAR
      nodes.
   -- defaults are propagated downward and may grow as more variables are bound
      along the path.
   -- Every andor node has a path and that path uniquely identifies that node. Thus
      paths in the tree and nodes are in 1-1 correspondence.
   -- a valid path and a ruleno uniquely identify a subpattern occurrence
   -- a path is _valid_ for a pattern space if it correctly designates a point
      in the space, i.e. once can follow the path to a point/node in the space
      (selection indices are in bounds, choice keys are consistent with node types,
      etc.).
*)
datatype andor
  = AND of  (* record or tuple pattern/type *)
    {path : path,  (* unique path to this node *)
     asvars: asbindings, (* at _this_ node *)
     vars : varbindings, (* vars bound at this node *)
     defaults : ruleset, (* rules live because of variable match here or above on path *)
     live : ruleset, (* INVARIANT: defaults subset live *)
     children: andor list}
     (* record, tuple, or _implicit_ tuple of constructor arguments,
      * or _implicit_ tuple of elements of a vector *)
  | OR of  (* datatype, vector, or constant pattern/type *)
    {path : path,  (* unique path to this node *)
     asvars : asbindings, (* at _this_ node *)
     vars : varbindings, (* vars bound at this node *)
     defaults : ruleset, (* rules live because of variable match here or above on path *)
     live : ruleset, (* INVARIANT: defaults subset live *)
     children : orKind}
  | VARS of  (* variables *)
    {path : path,  (* unique path to this node *)
     vars: varbindings   (* the variables bound at this point *)
     asvars: asbindings}
     (* a terminal node containing a variable
      * that has not (yet) been merged with an AND or OR node
      * the implicit live ruleset is the set of second elements or varbindings
      * A var node (implicitly) has the same live set as its parent. *)
	
and orKind
  = ORdata of dataChild list
     (* invariant: not(null(dataChild list)) *)
  | ORvec of Types.ty * vecChild list
     (* invariant: not(null(vecBranch list)) 
      * ty argument is vector element type *)
  | ORconst of constChild list
     (* invariant: not(null(constChild list)) *)
			  
withtype
    dataLink = dataCon * andor option * R.ruleset
         (* The andor option provides the andor tree for the argument pattern
          * for dataCon, if any (i.e. NONE if dataCon is a constant).
          * could replace dataCon with datacon and have tyvar list as a separate
          * component, if it is indeed needed *)
         (* ASSERT: if andor option is SOME andor, then ruleset = andor.live *)
and vecLink = int * andor * R.ruleset
         (* andor is an AND. vecLinks are ordered by the int (length) field *)
         (* ASSERT: ruleset == andor.live  -- redundancy *)
and constLink = constCon * R.ruleset
         (* no subsidiary (argument) patterns, so no andor
	  * ruleset is not redundant, but may not be needed *)

(* it is possible for an andor node to be modified by _both_ AS and VARS:
for example:
   (1) x as true
   (2) y
produces an andor tree with root node OR{path=[],asvars=[(x,1)], vars=[(y,2)], ...}.
It might be possible to merge VARS and AS into a single variable binding 
construct. But their behaviour is not equivalent, since layered bindings
to not generate "default" rules and simple variable bindings do.
*)
			
(* constantEq: constCon * constCon -> bool
 * constantEq(p1,p2) iff p1 and p2 are equal pattern constants *)
fun constantEq (DATApcon (d1, _), DATApcon (d2, _)) = conEq(d1, d2)
  | constantEq (INTconst n1, INTconst n2) = IntConst.same(n1,n2)
  | constantEq (WORDconst n1, WORDconst n2) = IntConst.same(n1,n2)
  | constantEq (STRINGconst s1, STRINGconst s2) = (s1 = s2)
  | constantEq _ = false


(* ================================================================================ *)
(* choice lists (formerly "decision" lists) -- flattened AND-OR trees *)

(* 
Paths identify a location in a structured pattern. In different rules, there
may be different subpatterns at this location, or no subpattern at all.  For
instance, consider

(1)  nil
(2)  cons(1, nil)

The second nil is at path SELECTR(2,DATAP(cons,ROOT)) (or R2:D[cons] for short).
There is of course no such path in pattern (1).

INVARIANT: the path of a BND element and the paths of the following decisions
are (sometimes) related in that the BND path is an prefix (not necessarily proper) of the
decisions that follow it in the decision list -- IF there is some structure at
this path location in some other rule.  If the next decision is a CHOICE,
it will have the same path as the BND that precedes it.  If the next "decision"
is a BND (as for the pattern (x,y), for instance), then the paths of the adjacent
BNDS are not comparable.

Should the BND be a separate element of the choice list (it is _not_ a choice),
or should it be a "wrapper" annotating a decision? I.e. should it have a decision
argument?

If the BNDs are elements of a choice list, is their position in that list significant?
For instance, if a BNDs element is followed by a choice element, is that BND element
viewed as "applying to" or "modifying" or "augmenting" that particular choice.  In that
case, should the BND element and its following choice be connected.

For instance,

   x as cons(y,nil).

Or what about something like 

   x as (y, nil)

where the BND is followed by a flattened AND node.

Or what about a merged pair of corresponding patterns:

   (1)  cons(x, nil)
   (2)  y

Could two BND elements with same path could be merged into one?

fun mergeBND(BND(p1,rs1), BND(p2,rs2)) =
    if eqPath(p1,p2)
    then BND(p1, R.union(rs1,rs2)
    else raise Fail "mergeBND"

* Default rulesets:
default rulesets at a choice contain the rulenos that will "survive" the choice,
in the sense that they remain live in all the children of the choice. ???

	
(* decision: decision "trees" *)
(* INVARIANT: in each case, length choices >= 1 *)
datatype orKind
  = ORdata  of (datacon  * ruleset * choice list) list
  | ORvec   of (int      * ruleset * choice list) list (* int is vector length *)
  | ORconst of (constCon * ruleset)               list

and choice
    = BND of     (* a variable binding point -- not a choice point! *)
        path *   (* a location where one or more variables occur *)
        ruleset  (* rulenos of the patterns in which they occur *)
    | CHOICE of  (* was CASEDEC *)
       {choices : orKind,
	path : path,
	defaults : ruleset}  (* rules not "affected" by this choice,
			      * because they have a variable at this path location ??? *)

(* NOTE: BND does not specify what variables occur at this path location in these rules.
 * Those variables (or their lvars) will have to recovered later.
 *)	 

(* decision tree *)
datatype dectree
  = DCHOICE of  (* was CASETEST *)
     {path : path,
      choices : decChoiceKind
      default : dectree option}
  | DBND of (* was BIND *)
     {path : path,
      dectree : dectree}
  | RHS of int  (* rule chosen *)

and decChoiceKind
  = Dch of (datacon * dectree) list
  | Vch of (int * dectree) list
  | Cch of (constCon * dectree) list
	       
(* mkRECORDpat : Absyn.pat -> Absyn.pat list -> Absyn.pat *)
(* replace field patterns in a record pat (1st arg) with elements of 2nd arg;
 * used only in MatchComp$orExpand *)
fun mkRECORDpat (RECORDpat{fields, flex=false, typ, ...}) pats =
      RECORDpat {flex=false, typ=typ,
                 fields=ListPair.map(fn((id,_),p)=>(id,p))(fields,pats)}
  | mkRECORDpat (RECORDpat{flex=true,...}) _ =
      bug "mkRECORDpat - flex record"
  | mkRECORDpat _ _ = bug "mkRECORDpat - non-record"


(* lvarEnv: association list mapping paths to lvars *)
type lvarEnv = (path * LV.lvar) list
fun lookupPath (a: path, (b,c)::d : lvarEnv) : LV.lvar =
       if pathEq(a,b) then c else lookupPath(a, d)
  | lookupPath _ = bug "lookupPath nil 2nd arg"

end (* toplevel local *)
end (* structure MCCommon *)

(* these functions moved from MCCommon to TypesUtil, but with the names:
   eqDatacon, dataconSign, dataconIsConst

fun eqDcon(DATACON{rep=a1,...}: datacon, DATACON{rep=a2,...}: datacon) = (a1 = a2)

fun signOfDatacon (DATACON{sign,...}) = sign
fun constOfDatacon (DATACON{const,...}) = const
 *)
		       
(* old definitions

(* old version of path *)
datatype path
  = RECORDPATH of path list
  | PIPATH of int * path        (* index into a product at path *)
  | VLENPATH of ty * path       (* vector *)
  | VPIPATH of int * ty * path  (* vector + index? *)
  | DELTAPATH of pcon * path    (* selecting a branch for an OR node *)
  | ROOTPATH
	
(* decision: decision "trees" *)
datatype decision
  = ALT of
     {path : path,
      sign : DA.consig,
      cases : dec_case list,
      defaults: ruleset}
  | BND of path * ruleset (* * decision ??? *)
withtype dec_case = pcon * ruleset * decision list

(* decision: decision "trees" *)
datatype decision
  = ALTD of
     {path : path,
      alts : dataAlt list,
      defaults: ruleset}
  | ALTC of
     {path : path,
      alts : constAlt list,
      defaults: ruleset}
  | ALTV of
     {path : path,
      alts : vecAlt list,
      defaults: ruleset}
  | BND of path * ruleset (* * decision ??? *)
withtype dataAlt  = datacon * ruleset * decision list
     and vecAlt   = int * ruleset * decision list
     and constAlt = constCon * ruleset

*)
