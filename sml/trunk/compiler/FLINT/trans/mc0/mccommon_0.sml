(* mccommon.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* TODO: this module requires a signature ! *)

structure MCCommon =
struct

local
  structure EM = ErrorMsg
  structure DA = Access
  structure LV = LambdaVar
  structure TU = TypesUtil
  structure V = VarCon
  open Types VarCon PLambda Absyn
in

(* --------------------------------------------------------------------------- *)
(* cons, links, and paths *)

datatype con   (* case descriminants *)
  = DATAcon of datacon * tyvar list
  | INTcon of int IntConst.t
  | WORDcon of int IntConst.t
  | STRINGcon of string
  | VLENcon of int * ty (* will be replaced by INTcon *)

datatype conKind
  = CONSTk (* con is constant (INT, WORD, STRING) or nullary data constructor *)
  | DCONk  (* con is non-nullary data constructor *)
  | VLENk  (* con is VLENcon _ *)

datatype link
  = PI of int         (* record/tuple selection *)
  | VPI of int * ty   (* vector selection *)
  | VLEN of ty        (* "phantom" vector length "discriminant"; needed in vector OR path
                         A VLEN link "links to" the CASETEST (on length) for a vector. *)
  | CON of con        (* datacon/constant discriminant (unary or nullary) *)

(* NOTE: the VLEN link for a vector length descrimination does not "destruct" the
 * vector in the way that a CON (datacon) link corresponds to destructing the
 * matching value. constant links also do not involve "destructing" the value. 
 * PI and VPI represent selection from a value, while VLEN and CON represent
 * discrimination of different sorts of value as well as (in some cases), destruction
 * of the value. *)

(* path: the reverse of the _path_ from root to a given node. uniquely identifies
 * the andor/pattern-space node at the (reverse of the) path's position.  Paths
 * are "linear" but a pattern space corrseponds to a "prefix"-closed set of paths.
 * If two paths are not prefixes of one another (i.e. they differ at some link)
 * they are on different "branches of the pattern space tree).
 * A path is maximal (terminal) in the pattern space if there are no extensions
 * of that path in the pattern space.  The node designated by a terminal path is
 * an andor LEAF node, reprsenting a constant or variable at that pattern point. *)

type path = link list

fun pathLength path = length path

(* ruleno and rule sets *)

structure RuleSet = IntListSet

type ruleno = RuleSet.item
type ruleset = RuleSet.set


(* --------------------------------------------------------------------------- *)
(* andor trees *)

type varBindings = (ruleno * V.var) list

datatype 'a subcase  (* kinds of andor0/andor subcases *)
  = CONST            (* discriminant is int, word, string, or constant datacon *)
  | DCON of 'a       (* discriminant is non-constant datacon, 'a is its argument *)
  | VEC of 'a list   (* discrimanant is vector length, 'a list are the vector elements *)

(* "proto AndOr trees *)
datatype andor0
  = AND0 of
     {bindings : varBindings,
      children : andor0 list}
  | OR0 of
     {bindings : varBindings,
      sign : DA.consig,
      cases : variant0 list}
  | LEAF0 of
     {bindings : varBindings}
withtype variant0 = con * ruleset * andor0 subcase

(* The "full" andor, with paths and binding rulesets (brule) and defaults *)
datatype andor
  = AND of  (* implicit record type, #fields = length children *)
     {path: path,             (* location in andor tree (old BINDDEC) *)
      brules: ruleset,        (* rules having variable bindings here (old BINDDEC) *)
      children: andor list}  (* tuple/record components *)
  | OR of   (* case descrimination *)
     {path: path,
      brules: ruleset,   (* rules having variable bindings here; relation with defaults? *)
      sign : DA.consig,  (* needed in CASETEST, to be passed eventually to genswitch *)
      defaults: ruleset, (* "default" rules (how defined? how used?) *)
      cases: variant list} (* case "variants" *)
  | LEAF of
     {path: path,
      brules: ruleset}
withtype variant =  con * ruleset * andor subcase


(* --------------------------------------------------------------------------- *)
(* decision tree *)
datatype dectree
  = CHOICE of
      {path: path,
       sign: DA.consig,  (* only needed to determine datatype width, but passed to SWITCH
                          * in Generate..genswitch *)
       cases: (con * dectree) list,
       default:  dectree option}  (* default if CHOICE is not saturated *)
  | BIND of path * dectree  (* not used until Gemerate.generate ("code" generation) *)
  | RHS of int  (* leaf node of decision tree dispatching to rule n *)


(* --------------------------------------------------------------------------- *)
(* LHS and RHS rule representations, after preprocessing LHS *)

type ramifiedLHS = (pat * path list * lvar) list
(* ramified left-hand side of a rule, where the initial pattern may have been
 * split into several patterns by OR expansion. Path list gives locations of
 * bound variables in corresponding pattern. All the lvars in the lists are the
 * same, and this lvar connects the ramified LHS with the corresponding RHS expression,
 * paired with the same lvar.
 *)

(* represenation of a single rule after preprocessing LHS pattern.
 * lvar components will agree, forming a linkage between LHS and RHS *)
type matchRep = (ramifiedLHS * (lvar * PLambda.lexp)) list


(* ================================================================================ *)
(* match compiler utility definitions *)

fun bug s = EM.impossible ("MCCommon: " ^ s)

fun mkRECORDpat (RECORDpat{fields, flex=false, typ, ...}) pats =
      RECORDpat {flex=false, typ=typ,
                 fields=ListPair.map(fn((id,_),p)=>(id,p))(fields,pats)}
  | mkRECORDpat (RECORDpat{flex=true,...}) _ =
      bug "mkRECORDpat - flex record"
  | mkRECORDpat _ _ = bug "mkRECORDpat - non-record"

fun conKind (DATAcon (d,_)) =
      if TU.dataconIsConst d then CONSTk else DCONk
  | conKind (INTcon _ | WORDcon _ | STRINGcon _) = CONSTk
  | conKind (VLENcon _) = VLENk

fun conEq (DATAcon (d1, _), DATAcon (d2, _)) = TU.eqDatacon (d1, d2)
  | conEq (INTcon n, INTcon n') = (#ival n = #ival n')   (* types assumed compatible *)
  | conEq (WORDcon n, WORDcon n') = (#ival n = #ival n') (* types assumed compatible *)
  | conEq (STRINGcon s, STRINGcon s') = (s = s')
  | conEq (VLENcon (n, _), VLENcon (n', _)) = (n = n')   (* types assumed compatible *)
  | conEq _ = false

(* linkEq : lint * link -> bool *)
(* link equality. The type of the node at the end of any given link is determined
 * by the path up to and including that link.  If we are comparing two links in the
 * context of a common prefix path up to those links, the types (e.g. of vectors for
 * two VLEN links) will be the same. *)
fun linkEq (PI i1, PI i2) = (i1 = i2)
  | linkEq (VPI (i1,_), VPI (i2,_)) = (i1 = i2)
  | linkEq (VLEN _, VLEN _) = true    (* vector types assumed compatible *)
  | linkEq (CON c1, CON c2) = conEq (c1, c2)
  | linkEq _ = false

fun pathEq (path1, path2) = ListPair.allEq linkEq (path1, path2)


(* path-to-lvar environments (alist with path{pathEq} as key) *)

type pathLvarEnv = (path * LV.lvar) list

fun bindPath (path: path, lvar : LV.lvar, env : pathLvarEnv) : pathLvarEnv =
    (path, lvar) :: env

fun lookupPath (path: path, (path', lvar) :: rest : pathLvarEnv) : LV.lvar =
      if pathEq(path, path') then lvar else lookupPath(path, rest)
  | lookupPath (_, nil) = bug "lookupPath: unbound path"

end (* toplevel local *)
end (* structure MCCommon *)
