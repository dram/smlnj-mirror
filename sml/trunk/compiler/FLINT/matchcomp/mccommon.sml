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
	     
(* AND-OR trees.
   -- There is one form of AND node, implying matching of all subnodes, used
      for records, tuples, and the elements of a vector,
   -- There are three forms of OR or "choice" nodes: ORdata, ORvec, and ORconst,
      descriminating among datatype datacons of a given datatype, vector lengths,
      or primitive constants (int, word, char?, and string).
   -- There is one kind of terminal node:  VAR. This has
      no pattern structure beneath it.  ORconst nodes have children, but
      the children are teminal "nodes". So are dataChild links associated
      with constant data constructors (e.g. nil).
*)
datatype andor
  = AND of andor list
     (* record, tuple, or _implicit_ tuple of constructor arguments,
      * or _implicit_ tuple of elements of a vector *)
  | ORdata of dataChild list
     (* invariant: not(null(dataChild list)) *)
  | ORvec of vecChild list
     (* invariant: not(null(vecBranch list)) 
      * could add ty argument (vector element type) if needed *)
  | ORconst of constChild list
     (* invariant: not(null(constChild list)) *)
  | VAR
     (* a terminal node containing a variable
      * the variable bound here will be given in a wrapped VARS *)
  | AS of andor * asBindings
     (* "as" bound variables. Match the andor _and_ bind the appropriate var
      * from asBindings *)
  | VARS of andor * varBindings
     (* match the andor, _or_ any of the default vars in varBindings
      * representing var patterns at this position in the pattern space. *)

withtype
    dataChild = dataCon * andor option * R.ruleset
         (* The andor option provides the andor tree for the argument pattern
          * for dataCon, if any (i.e. NONE if dataCon is a constant).
          * could replace dataCon with datacon and have tyvar list as a separate
          * component, if it is indeed needed *)
and vecChild = int * andor * R.ruleset
         (* andor is an AND. vecBranches are ordered by the int (length) field *)
and constChild = constCon * R.ruleset
         (* no subsidiary (argument) patterns, so no andor  *)

(* it is possible for an andor node to be modified by _both_ AS and VARS:
for example:
   (1) x as true
   (2) y
produces VARS(AS(DATA ..., [(x,1)]),[(y,2)])
It is probably possible to merge VARS and AS into a single variable binding 
construct. But their behaviour is not entirely equivalent, since AS bindings
to not generate "default" rules and VARS bindings do.
*)
			
(* constantEq: constCon * constCon -> bool
 * constantEq(p1,p2) iff p1 and p2 are equal pattern constants *)
fun constantEq (DATApcon (d1, _), DATApcon (d2, _)) = conEq(d1, d2)
  | constantEq (INTconst n, INTconst n') = IntConst.same(n,n')
  | constantEq (WORDconst n, WORDconst n') = IntConst.same(n,n')
  | constantEq (STRINGconst s, STRINGconst s') = s = s'
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

* Path constructor name changes (from the 110.97 version):
   RECORDPATH not used, eliminated
   VLENPATH not used, eliminated (replaced by VECP)
   PIPATH ==> SELECTR   -- record element selection
   VPIPATH ==> SELECTV  -- vector element selection (why not use SELECTR?)
   DELTAPATH ==> CHOICED, CHOICEC, CHOICEV
   CHOICEC not used?  No subsidary nodes beneath a constant pattern
   Do we really need two selection constructors, SELECTR and SELECTV?
     -- do we need to propagate an element type for vector patterns?

*)

datatype link
  = RL  of int       (* index into a product "located at" path, 0-based *)
  | VL  of int (* * ty *)  (* index into an element of a vector. Why do we need ty? *)
  | DL  of dataCon   (* selecting a branch of an DATA node on datacon *)
  | CL  of constCon  (* selecting a branch of an CONST node on const. Not used? *)
  | VLL of int       (* selecting a branch of an VEC node on vector length *)

type rpath = link list (* links are in reverse order from node to root *)
type path = link list (* links ordered from root to node *)
*)

fun linkEq (RL i1, RL i2) = i1 = i2
  | linkEq (LV i1, LV i2) = i1 = i2
  | linkEq (VLL i1, VLL i2) = i1 = i2
  | linkEq (DL c1, DL c2) = TU.eqDatacon(c1,c2)
  | linkEq (CL c1, CL c2) = constConEq
  | linkEq _ = false

fun pathEq(link1::rest1, link2::rest2) = linkEq(link1,link2) andalso pathEq(rest1,rest2)
  | pathEq(nil,nil) = true
  | pathEq _ = false

val pathEq = ListPair.allEq linkEq
	
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
