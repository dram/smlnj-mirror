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
			  
(* keys: most keys are used to designate choices in the "variants" field of OR nodes,
 *   but there is an extra R (for record) representing record/product selection.
 *   These key values (appearing in variants) distinguish the different flavors
 *   of OR nodes (data, vector and 4 varieties of constants). *)
datatype key
  = D of (T.datacon * T.tyvar list)  (* content of "dataCon" *)
  | V of int * ty (* -- vector length and element type, a kludge
                   * all vector keys in a node have same ty -- redundancy *)
  | I of int IntConst.t  -- superceding type constCon 
  | W of int IntConst.t
  | C of char
  | S of string
  | R of int    (* not a choice key, but a selection key for products,
                 * will only appear in paths, not in variants *)

(* eqKey : key * key -> bool
 * type info disregarded when comparing Dkeys and Vkeys *)
fun eqKey (D(dcon1,_), D(dcon2,_)) = TU.dataconEq(dcon1,dcon2)
  | eqKey (V(l1,_), V(l2,_)) = l1 = l2
  | eqKey (I c1, I c2) = IntConst.same(c1,c2)
  | eqKey (W c1, W c2) = IntConst.same(c1,c2)
  | eqKey (C c1, C c2) = c1 = c2
  | eqKey (S s1, S s2) = s1 = s2
  | eqKey (R i1, R i2) = i1 = i2
  | eqKey _ = false  (* mismatching key constructors *)

(* ================================================================================ *)
(* paths: 
   paths locate points in the "pattern space" determined by a sequence of patterns
      (a subtree of the complete pattern tree determined by the common type of
      the patterns). Points in the pattern space correspond to andor nodes, which
      therefore have a unique identifying path.
*)

(* a path is well formed only if its links are well formed *)
(* NOTE: maybe links should be subsumed by choiceKeys? *)
type rpath = key list (* keys are in reverse order from node to root *)
type path = key list (* keys ordered from root to node *)

(*
fun pathEq(link1::rest1, link2::rest2) = linkEq(link1,link2) andalso pathEq(rest1,rest2)
  | pathEq(nil,nil) = true
  | pathEq _ = false
*)
val pathEq = ListPair.allEq eqKey

val rootPath : path = []

(* extendPath : path * key -> path *)
(* extends a path with a new link (key) at the end *)
fun extendPath (p, k) = p @ [k]   (* expensive, but paths are normally short *)
fun extendRPath (p, k) = k::p  (* cheap *)

(* incompatible : path * path -> bool *)
(* Two paths are incompatible if they diverge at a choice (OR) node.
 * Paths that diverge at a product node (diff first at R links) are
 * compatible; paths that are prefix comparable are compatible. *)
fun incompatible (k1::rest1, k2:rest2) =
      if eqKey(k1,k2) then incompatible(rest1, rest2)
      else (case k1
	     of R _ => false
	      | _ => true)
  | incompatible (nil, path2) = false
  | incompatible (path1, nil) = false

(* potentially useful functions on paths:

pathPrefix: path * path -> bool  (* prefix ordering *)
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
    {lvar: lvar,               (* lvar to be bound to value at this point *)
     path : path,              (* unique path to this node *)
     asvars: asbindings,       (* at _this_ node *)
     vars : varbindings,       (* variables bound at this point *)
     live : ruleset,           (* live rules *)
     defaults : ruleset,       (* rules matching be default (vars) *)
     children: andor list}     (* tuple components as children -- AND node *)
  | OR of (* datatype, vector, or constant pattern/type *)
    {lvar: lvar,
     path : path,              (* unique path to this node *)
     asvars: asbindings,       (* layered variable at _this_ node *)
     vars : varbindings,       (* variables bound to this point, with rule no. *)
     live : ruleset,           (* rule patterns matchable at this point *)
     defaults: ruleset,        (* rules matching here by default (vars) *)
     variants: variant list} (* the branches/choices of OR node; non-null *)
  | SINGLE of  (* singular datacon app, a kind of no-op for pattern matching *)
    {lvar : lvar,
     path : path,              (* unique path to this node *)
     asvars: asbindings,       (* at _this_ node *)
     vars: varbindings,        (* variables bound to this point *)
     dcon: dcon,               (* the singleton dcon of the datatype for this node *)
     arg: andor}               (* arg of the dcon, LEAF if it is a constant *)
  | VARS of  (* a node occupied only by variables *)
    {lvar: lvar,
     path : path,              (* unique path to this node *)
     asvars: asbindings,       (* at _this_ node *)
     vars: varbindings,        (* Invariant: live = map #2 vars ?? *)
     live: ruleset}            (* rules live at this point ??? *)
	(* should VARS have a defaults field? == map #2 vars + inherited from the path *)
  | LEAF of   (* leaf, with live rules, used in place of "arg" of constant dcon *)
    {path: path,
     live: ruleset,
     defaults: ruleset}
(*  | INITIAL   (* initial empty andor into which patterns are merged *) *)

withtype variant = key * andor
(* this pushes the discrimination of the OR-kind into the keys of the variants. *)

			  
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

followPath : path * andor -> andor
(the andor subtree located at path in the given andor tree *)

pathToType : path * ty -> ty  (* don't need a node, path suffices *)
nodeType : andor -> ty

partial: andor -> bool
   andor is an OR node, and partial(node) is true if 
   (1) variants is ORdata and not all construtors appear in variants list
   (2) ORvec or ORconst, which are inherently partial coverage

orBreadth : andor -> int option
(number of children of an OR node, NONE for non-OR nodes

*)

(* path : andor -> path *)
fun path(AND{path,...}) = path
  | path(OR{path,...} = path
  | path(SINGLE{path,...}) = path
  | path(VARS{path,...}) = path
  | path(LEAF{path,...}) = path

fun findKey (key, (key'::n)::rest) =
    if eqKey(key,key') then SOME n
    else findKey(key, rest)
  | findKey nil = NONE

fun getNode(andor, _, 0) = andor
  | getNode(andor, nil, _) = andor
  | getNode(andor, key::path, depth) =
    (case (andor,key)
      of (AND children, R i) =
	 getNode(nth(children, i),path,depth-1)
       | (OR{variants,...},key) =>
	 (case findKey(key,variants)
	    of NONE => bug"getNode"
	     | SOME node => getNode(node, path, depth-1))
       | (SINGLE{arg,...}, key) =>
	 getNode(arg, path, depth-1)
       | (VARS _. LEAF _) => bug "getNode")

(* parentNode: andor * andor -> andor *)
fun parent (andor, root) =
    let val path = path(andor)
        val d = length(path) -1
     in getNode(root, path(andor), d)
    end  

end (* structure MCTypes *)
