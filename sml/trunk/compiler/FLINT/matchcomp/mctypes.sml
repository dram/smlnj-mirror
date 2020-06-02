(* mctypes.sml *)
(* types for the match compiler
 * replaces older versions of the file named mccommon.sml *)

structure MCTypes =
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

type ruleno = R.ruleno    (* == int, the index number of a rule in the match, zero-based *)
type ruleset = R.ruleset  (* == IntBinarySet.set *)
   (* a set of rule numbers, maintained in strictly ascending order without duplicates *)

type binding = var * ruleno
   (* a variable bound at some point in the given rule, either as a
    * basic var pattern (VARpat) or through an "as" pattern (LAYEREDpat) *)
type varBindings = binding list  (* variables bound by VARpat *)		    
type asBindings = binding list   (* variables bound by LAYEREDpat, i.e. an "as" pattern *)
			  
(* datatype constructors in patterns *)
type dataCon = datacon * T.tyvar list   (* do we need the tyvar list (bound tyvars)? *)

datatype constCon
  = INTconst of int IntConst.t
  | WORDconst of int IntConst.t
  | STRINGconst of string
  | CHARconst of char
     (* avoid treating chars as int -- leave them as characters *)
	     
(* eqConst : constCon * constCon -> bool *)
fun eqConst (INTconst v1, INTconst v2) = IntConst.same(v1, v2)
  | eqConst (WORDconst v1, WORDconst v2) = IntConst.same(v1, v2)
  | eqConst (CHARconst v1, CHARconst v2) = v1 = v2
  | eqConst (STRINGconst v1, STRINGconst v2) = v1 = v2
							
(* ================================================================================ *)
(* paths: 
   paths locate points in the "pattern space" determined by a sequence of patterns
      (a subtree of the complete pattern tree determined by the common type of
      the patterns). Points in the pattern space correspond to andor nodes, which
      therefore have a unique identifying path.
*)

datatype link
  = RL  of int       (* select ith component of a product/record *)
  | DL  of dataCon   (* choosing a branch of an datatype node on datacon *)
  | CL  of constCon  (* choosing a branch of an constant node on const. Not used? *)
  | VL of int        (* choosing a branch of an vector node with vector length i *)

type rpath = link list (* links are in reverse order from node to root *)
type path = link list (* links ordered from root to node *)

fun linkEq (RL i1, RL i2) = i1 = i2
  | linkEq (VL i1, VL i2) = i1 = i2
  | linkEq (DL c1, DL c2) = TU.eqDatacon(c1,c2)
  | linkEq (CL c1, CL c2) = eqConst (c1,c2)
  | linkEq _ = false

(*
fun pathEq(link1::rest1, link2::rest2) = linkEq(link1,link2) andalso pathEq(rest1,rest2)
  | pathEq(nil,nil) = true
  | pathEq _ = false
*)
val pathEq = ListPair.allEq linkEq

val rootPath : path = []

(* addPath : path * link -> path *)
fun addPath (p, l) = l @ [p]   (* expensive, but paths are normally short *) 

(* potentially useful functions on paths:

pathPrefix: path * path -> bool
pathAppend: path * path -> path
*)
			    
(* ================================================================================ *)
(* andor trees *)
(* Version modified based on toy-mc/matchtree.sml.
Differs from old andor tree: Single (for singlton datacons), Initial, starting
place for merging patterns (no initAndor needed), and Leaf, which seems to be used
as a phantom argument for nullary dcons. (?) *)

(* Do we need to, or would it be useful to, explicitly include the type
 * for each node. Given the type is known before match compilation, it would
 * be easy to propagate types down through the constructed AND-OR tree.
 * How would we use such types? 
 *   Could also maintain a mapping from paths to types of the nodes designated
 * by those paths.
 *)

(* keys for "variants" lists of choices 
 *   These key values could be used to distinguish the different flavors of OR nodes *)
datatype choiceKey
  = DATAkey of (T.datacon * T.tyvar list)  (* content of "dataCon" *)
  | VECTORkey of int * ty (* -- vector length and element type, a kludge *)
  | CONSTkey of ConstCon
(*| INTkey of int IntConst.t  -- superceding type constCon 
  | WORDkey of int IntConst.t
  | CHARkey of char
  | STRINGkey of string
*)
		    
datatype andor
  = AND of   (* product pattern *)
    {path : path,              (* unique path to this node *)
     asvars: asbindings,       (* at _this_ node *)
     vars : varbindings,       (* variables bound at this point *)
     live : ruleset,           (* live rules *)
     defaults : ruleset,       (* rules matching be default (vars) *)
     children: andor list}     (* tuple components as children -- AND node *)
  | OR of (* datatype, vector, or constant pattern/type *)
    {path : path,              (* unique path to this node *)
     asvars: asbindings,       (* layered variable at _this_ node *)
     vars : varbindings,       (* variables bound to this point, with rule no. *)
     live : ruleset,           (* rule patterns matchable at this point *)
     defaults: ruleset,        (* rules matching here by default (vars) *)
     variants: orKind, (* the branches -- an OR node *)
  | SINGLE of  (* singular datacon app, a kind of no-op for pattern matching *)
    {path : path,              (* unique path to this node *)
     asvars: asbindings,       (* at _this_ node *)
     vars: varbindings,        (* variables bound to this point *)
     dcon: dcon,               (* the singleton dcon of the datatype for this node *)
     arg: andor}               (* arg of the dcon, LEAF if it is a constant *)
  | VARS of
    {path : path,              (* unique path to this node *)
     asvars: asbindings,       (* at _this_ node *)
     vars: varbindings,        (* Invariant: live = map #2 vars ?? *)
     live: ruleset}            (* rules live at this point ??? *)
	(* should VARS have a defaults field? == map #2 vars + inherited from the path *)
  | LEAF of   (* leaf, with live rules, used in place of "arg" of constant dcon *)
    {path: path, live: ruleset}
(*  | INITIAL   (* initial empty andor into which patterns are merged *) *)

and orKind
  = ORdata of (dataCon * andor) list
  | ORvec of T.ty * (int * andor) list
     (* int is vector pattern length for each entry in list
      * ty is vector element type
      * elements are sorted by length
      * andor elements all AND (tuple of vector elements) *)
  | ORconst of (constCond * andor) list
(* ASSERT: arguments of ORdata, ORvec, ORconst are non-null 
 * INVARIANT: the andor of an ORconst variant is always a LEAF, and the main
 *   purpose of that LEAF node is to record a live ruleset for that position *)

withtype variant = (choiceKey * andor) list
(* this pushes the discrimination of the OR-kind into the keys of the variants.
 * what is the impact on the andor tree construction? *)

			  
(* potentially useful functions:
 
(* a datatype that could be viewed as an alternative to including the type of a node *)
datatype nodeKind
 = DATAkind of T.ty (* type of datacon keys *)  (* OR datatype node (or SINGLE?) *)
 | VECTORkind of ty (* element type *)          (* OR vector node *)
 | INTkind | WORDkind | CHARkind | STRINGkind (* OR constant nodes)
 | VARSkind
 | LEAFkind  (* terminal below a constant *)
 | ANDkind 

nodeKind : andor -> nodeKind (type)
eqNode : andor * andor -> bool
(two nodes are equal if their path component is equal, needed only for OR nodes?)

findPath : path * andor -> andor
(the andor subtree located at path in the given andor tree *)

pathToType : path * ty -> ty

partial: andor -> bool
   andor is an OR node, and partial(node) is true if 
   (1) variants is ORdata and not all construtors appear in variants list
   (2) ORvec or ORconst, which are inherently partial coverage

orBreadth : andor -> int option
(number of children of an OR node, NONE for non-OR nodes *)

*)


end (* structure MCTypes *)
