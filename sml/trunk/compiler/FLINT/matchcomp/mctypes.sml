(* mctypes.sml *)
(* types for the match compiler
 * replaces older versions of the file named mccommon.sml *)

structure MCTypes =
struct

local
  structure EM = ErrorMsg
  structure LV = LambdaVar
  structure T = Types
  structure TU = TypesUtil
  structure R = Rules
  structure VC = VarCon
  open VarCon Absyn
      (* also used/mentioned: IntConst, ListPair *)
in

fun bug s = EM.impossible ("MCTypes: " ^ s)

type ruleno = R.ruleno    (* == int, the index number of a rule in the match, zero-based *)
type ruleset = R.ruleset  (* == IntBinarySet.set *)
   (* a set of rule numbers, maintained in strictly ascending order without duplicates *)

type binding = VC.var * ruleno
   (* a variable bound at some point in the given rule, either as a
    * basic var pattern (VARpat) or through an "as" pattern (LAYEREDpat) *)
type varBindings = binding list  (* variables bound by VARpat *)		    
type asBindings = binding list   (* variables bound by LAYEREDpat, i.e. an "as" pattern *)
			  
(* keys: most keys are used to discriminate choices in the "variants" field of OR nodes
 *   and the decVariants of decision trees. These key values (appearing in variants)
 *   determine the different flavors of OR nodes (data, vector and 4 varieties of constants).
 *   But there is an extra R (for record) representing record/product selection. It appears
 *   only in paths to indicate product projections. *)
datatype key
  = D of (T.datacon * T.tyvar list)  (* datacon key, possibly constant *)
  | V of int * T.ty (* vector length (and element type -- a kludge, since
                     * all vector keys in a node have same ty -- redundancy *)
    
  (* following constant keys supercede previous constCon constructors *)
  | I of int IntConst.t  (* int constant *)
  | W of int IntConst.t  (* word constant *)
  | C of char            (* char constant *)
  | S of string          (* string constant *)
  
  | R of int    (* not a choice discriminator, but a selection key for products,
                 * will only appear in paths, not in variants *)

(* eqKey : key * key -> bool
 * type info disregarded when comparing Dkeys and Vkeys *)
fun eqKey (D(dcon1,_), D(dcon2,_)) = TU.dataconEq(dcon1,dcon2)
  | eqKey (V(l1,_), V(l2,_)) = l1 = l2
  | eqKey (I c1, I c2) = IntConst.same(c1,c2)
  | eqKey (W c1, W c2) = IntConst.same(c1,c2)
  | eqKey (C c1, C c2) = c1 = c2  (* character keys *)
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
type path = key list (* keys ordered from root to node *)
type rpath = key list (* keys are in reverse order from node to root *)

val pathEq = ListPair.allEq eqKey

val rootPath : path = []
val rootRPath : rpath = []

(* extendPath : path * key -> path *)
(* extendRPath : rpath * key -> rpath *)
(* extends a path with a new link (key) at the end *)
(* should pass rpath down while initializing andor tree, then
   reverse to a path when storing in the node *)
fun extendPath (p, k) = p @ [k]   (* expensive, but paths are normally short *)
fun extendRPath (p, k) = k::p     (* cheap *)
fun reversePath (p: path): rpath = rev p
fun reverseRPath (p: rpath): path = rev p

(* incompatible : path * path -> bool *)
(* Two paths are incompatible if they diverge at a choice (OR) node.
 * Paths that diverge at a product node (diff first at R links) are
 * compatible; paths that are prefix comparable are compatible. *)
fun incompatible (k1::rest1, k2::rest2) =
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

(* INVARIANT: In any variant (key, andor), getPath(andor) ends with key. *)
			    
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
 *   Do we need defaults fields for VARS and LEAF nodes?
 *)

datatype andor
  = AND of   (* product pattern *)
    {lvar: LV.lvar,            (* lvar to be bound to value at this point *)
     path : path,              (* unique path to this node *)
     asvars: asBindings,       (* layered variables bound at _this_ point *)
     vars : varBindings,       (* variables bound at this point *)
     direct : ruleset,         (* direct rules: rules with product pats at this pattern point *)
     defaults : ruleset,       (* rules matching matching because of variables along the path *)
     children: andor list}     (* tuple components as children -- AND node *)
  | OR of (* datatype, vector, or constant pattern/type *)
    {lvar: LV.lvar,
     path : path,              (* unique path to this node *)
     asvars: asBindings,       (* layered variables bound at _this_ point *)
     vars : varBindings,       (* variables bound to this point, with rule no. *)
     direct : ruleset,         (* rule patterns matchable at this point because of a key *)
     defaults: ruleset,        (* rules matching here by default (vars) *)
     variants: variant list}   (* the branches/choices of OR node; non-null *)
  | SINGLE of  (* singular datacon app, a kind of no-op for pattern matching *)
    {lvar : LV.lvar,
     path : path,              (* unique path to this node *)
     asvars: asBindings,       (* at _this_ node *)
     vars: varBindings,        (* variables bound to this point *)
     dcon: T.datacon,          (* the singleton dcon of the datatype for this node *)
     arg: andor}               (* arg of the dcon, LEAF if it is a constant *)
  | VARS of  (* a node occupied only by variables *)
    {lvar: LV.lvar,
     path : path,              (* unique path to this node *)
     asvars: asBindings,       (* at _this_ node *)
     vars: varBindings,        (* virtual: direct = map #2 vars *)
     defaults: ruleset}        (* rules matching here by default *)
  | LEAF of   (* used as variant andor for constants, with direct and default rules *)
    {path: path,
     direct: ruleset,
     defaults: ruleset}
  | INITIAL   (* initial empty andor into which initial pattern is merged
               * to begin the construction of an AND-OR tree *)

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

(* getPath : andor -> path *)
fun getPath(AND{path,...}) = path
  | getPath(OR{path,...}) = path
  | getPath(SINGLE{path,...}) = path
  | getPath(VARS{path,...}) = path
  | getPath(LEAF{path,...}) = path
  | getPath INITIAL = bug "getPath(INITIAL)"

(* getLvar : andor -> lvar *)
fun getLvar(AND{lvar,...}) = lvar
  | getLvar(OR{lvar,...}) = lvar
  | getLvar(SINGLE{lvar,...}) = lvar					    
  | getLvar(VARS{lvar,...}) = lvar					    
  | getLvar(LEAF _) = bug "getLvar(LEAF)"
  | getLvar INITIAL = bug "getLvar(INITIAL)"

(* getDirect : andor -> ruleset *)
fun getDirect(AND{direct,...}) = direct
  | getDirect(OR{direct,...}) = direct
  | getDirect(SINGLE{arg,...}) = getDirect arg 
  | getDirect(VARS{vars,...}) = R.fromList(map #2 vars)
  | getDirect(LEAF{direct,...}) = direct
  | getDirect INITIAL = bug "getDirect(INITIAL)"

(* getDefaults : andor -> ruleset *)
fun getDefaults(AND{defaults,...}) = defaults
  | getDefaults(OR{defaults,...}) = defaults
  | getDefaults(SINGLE{arg,...}) = getDefaults arg 
  | getDefaults(VARS{defaults,...}) = defaults
  | getDefaults(LEAF{defaults,...}) = defaults
  | getDefaults INITIAL = bug "getDefaults(INITIAL)"

(* getLive : andor -> ruleset
   live rules is union of direct and defaults *)
fun getLive(AND{direct,defaults,...}) = R.union(direct,defaults)
  | getLive(OR{direct,defaults,...}) = R.union(direct,defaults)
  | getLive(SINGLE{arg,...}) = getLive arg 
  | getLive(VARS{defaults,...}) = defaults  (* direct subset defaults *)
  | getLive(LEAF{direct,defaults,...}) = R.union(direct,defaults)
  | getLive INITIAL = bug "getLive(INITIAL)"


(* findKey : key * variant list -> andor option *)
fun findKey (key, (key',node)::rest) =
    if eqKey(key,key') then SOME node
    else findKey(key, rest)
  | findKey (_, nil) = NONE

(* getNode : andor * path * int -> andor *)
(* REQUIRE: for getNode(andor,path,depth): depth <= length path *)
fun getNode(andor, _, 0) = andor
  | getNode(andor, nil, _) = andor
  | getNode(andor, key::path, depth) =
    (case (andor,key)
      of (AND{children,...}, R i) =>
	   getNode(List.nth(children, i),path,depth-1)
       | (OR{variants,...},key) =>
	   (case findKey(key,variants)
	      of NONE => bug "getNode"
	       | SOME node => getNode(node, path, depth-1))
       | (SINGLE{arg,...}, key) =>
	   getNode(arg, path, depth-1)
       | ((VARS _ | LEAF _),_) => bug "getNode(VARS|LEAF)"
       | _ => bug "getNode arg")
	
(* parentNode: andor * andor -> andor *)
fun parent (andor, root) =
    let val path = getPath(andor)
        val d = length(path) - 1
     in getNode(root, path, d)
    end  


(* decision trees *)	
datatype decTree
  = DLEAF of ruleno    (* old version: RHS *)
     (* if you get to this node, bind variables along branch and dispatch to RHS(ruleno) *)
  | RAISEMATCH  (* probably redundant -- remove when sure *)
  | CHOICE of
    {node : andor,  (* an OR node used for dispatching *)
     choices : decVariant list,
     default : decTree option}
       (* one child for each variant of the node, + a default if node is partial *)
withtype decVariant = key * decTree


(* code *)
type lvar = LambdaVar.lvar

datatype mcexp
  = Var of lvar  (* how used? *)
  | Letr of lvar * lvar list * mcexp  (* to destructure an AND *)
  | Case of lvar * (key * lvar option * mcexp) list * mcexp option (* to destructure an OR *)
  | RHS of ruleno  (* dispatch to appropriate RHS *)
  | MATCH  (* raise a match exception -- may be redundant *)

				
end (* local *)
end (* structure MCTypes *)
