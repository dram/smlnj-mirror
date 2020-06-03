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
			  
(* keys for choices in the "variants" field of OR nodes *)
(*   These key values distinguish the different flavors of OR nodes *)
datatype choiceKey
  = DATAkey of (T.datacon * T.tyvar list)  (* content of "dataCon" *)
  | VECTORkey of int * ty (* -- vector length and element type, a kludge
                           * all vector keys in a node have same ty *)
  | INTkey of int IntConst.t  -- superceding type constCon 
  | WORDkey of int IntConst.t
  | CHARkey of char
  | STRINGkey of string

fun eqChoiceKey(DATAkey(dcon1,_), DATAkey(dcon2,_)) = TU.dataconEq(dcon1,dcon2)
  | eqChoiceKey(VECTORkey(l1,_), VECTORkey(l2,_)) = l1 = l2
  | eqChoiceKey(INTkey c1, INTkey c2) = IntConst.same(c1,c2)
  | eqChoiceKey(WORDkey c1, WORDkey c2) = IntConst.same(c1,c2)
  | eqChoiceKey(CHARkey c1, CHARkey c2) = c1 = c2
  | eqChoiceKey(STRINGkey s1, STRINGkey s2) = s1 = s2

(* ================================================================================ *)
(* paths: 
   paths locate points in the "pattern space" determined by a sequence of patterns
      (a subtree of the complete pattern tree determined by the common type of
      the patterns). Points in the pattern space correspond to andor nodes, which
      therefore have a unique identifying path.
*)

(* a link is well formed if the DL, CL, and VL constructors are applied to
 * choiceLinks of the right kind, i.e. DL(DATAkey _), VL(VECTORkey _) and
 * CL applied to either INTkey, WORDkey, CHARkey, or STRINGkey *)
datatype link
  = RL of int        (* select ith component of a product/record *)
  | DL of choiceKey  (* choosing a branch of an datatype node on datacon *)
  | CL of choiceKey  (* choosing a branch of an constant node on const. Not used? *)
  | VL of choiceKey  (* choosing a branch of an vector node with vector length i *)

(* a path is well formed only if its links are well formed *)
(* NOTE: maybe links should be subsumed by choiceKeys? *)
type rpath = link list (* links are in reverse order from node to root *)
type path = link list (* links ordered from root to node *)

fun linkEq (RL i1, RL i2) = i1 = i2
  | linkEq (VL k1, VL k2) = eqChoiceKey(k1,k2)
  | linkEq (DL k1, DL k2) = eqChoiceKey(k1,k2)
  | linkEq (CL k1, CL k2) = eqChoiceKey(k1,k2)
  | linkEq _ = false

(*
fun pathEq(link1::rest1, link2::rest2) = linkEq(link1,link2) andalso pathEq(rest1,rest2)
  | pathEq(nil,nil) = true
  | pathEq _ = false
*)
val pathEq = ListPair.allEq linkEq

val rootPath : path = []

(* addToPath : path * link -> path *)
fun addToPath (p, l) = l @ [p]   (* expensive, but paths are normally short *) 

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
     variants: (key * andor) list} (* the branches/choices of OR node; non-null *)
  | SINGLE of  (* singular datacon app, a kind of no-op for pattern matching *)
    {path : path,              (* unique path to this node *)
     asvars: asbindings,       (* at _this_ node *)
     vars: varbindings,        (* variables bound to this point *)
     dcon: dcon,               (* the singleton dcon of the datatype for this node *)
     arg: andor}               (* arg of the dcon, LEAF if it is a constant *)
  | VARS of  (* a node occupied only by variables *)
    {path : path,              (* unique path to this node *)
     asvars: asbindings,       (* at _this_ node *)
     vars: varbindings,        (* Invariant: live = map #2 vars ?? *)
     live: ruleset}            (* rules live at this point ??? *)
	(* should VARS have a defaults field? == map #2 vars + inherited from the path *)
  | LEAF of   (* leaf, with live rules, used in place of "arg" of constant dcon *)
    {path: path,
     live: ruleset,
     defaults: ruleset}

(*  | INITIAL   (* initial empty andor into which patterns are merged *) *)

type variant = (choiceKey * andor) list
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
nodeType : andor -> ty

partial: andor -> bool
   andor is an OR node, and partial(node) is true if 
   (1) variants is ORdata and not all construtors appear in variants list
   (2) ORvec or ORconst, which are inherently partial coverage

orBreadth : andor -> int option
(number of children of an OR node, NONE for non-OR nodes *)

*)

end (* structure MCTypes *)
