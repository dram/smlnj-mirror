(* new-mctypes-new.sml *)
(* types for the match compiler
 * replaces older versions of the file named mccommon.sml *)

(* new version where andor nodes do not have svars *)
(* new new version introducing "layers" *)

structure MCTypes =
struct

local
  structure EM = ErrorMsg
  structure LV = LambdaVar
  structure T = Types
  structure TU = TypesUtil
  structure R = Rules
  structure V = VarCon
  structure SV = SVar
  structure K = Key
  structure L = Layers
  structure LS = Layers.Set
  open Absyn
  (* also used/mentioned: IntConst, ListPair *)
in

fun bug msg = EM.impossible ("MCTypes: " ^ msg)

type rule = Absyn.pat * Absyn.exp
type ruleno = R.ruleno    (* == int, the index number of a rule in the match, zero-based *)
type ruleset = R.ruleset  (* == IntBinarySet.set *)
   (* a set of rule numbers (no duplicates) *)


type binding = V.var * L.layer
   (* a variable bound at some point in the given rule, either as a
    * basic var pattern (VARpat) or through an "as" pattern (LAYEREDpat) *)
type varBindings = binding list
   (* variables bound by VARpat or LAYEREDpat *)

(* ================================================================================ *)
(* paths:
   paths locate points in the "pattern space" determined by a sequence of patterns
      (a finite subtree of the complete pattern tree determined by the common type of
      the patterns). Points in the pattern space correspond to andor nodes, which
      have a unique identifying path.
*)

(* a path is well formed only if its links are well formed *)
type path  = K.key list (* keys ordered from root to node *)
type rpath = K.key list (* keys are in reverse order from node to root *)

val pathEq = ListPair.allEq K.eqKey

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

fun pathToString path =
    let fun ts [] = []
	  | ts [s] = [s]
	  | ts (s::ss) = s :: "." :: ts ss
     in concat("<":: ts (map K.keyToString path) @ [">"])
    end

type trace = path list

(* potentially useful functions on paths:
     pathPrefix: path * path -> bool  (* prefix ordering *)
     pathAppend: path * path -> path
 *)

(* INVARIANT: In any variant (key, andor), getPath(andor) ends with that key. *)

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
 *   Do we need live fields for VARS and LEAF nodes?  Yes (probably?).
 * LEAF nodes appear only in variants with constant keys (including constant dcons).
 *   Any var or as-var bindings at their position will be associated with the parent
 *   OR node. This is clearly right for vars occurring at the position, but what
 *   about a match like "(1) false; (2) x as true"?
 *)

(* There are 6 varieties of OR-node, distinguished by the value being used for discrimination,
   as specified by the variants of the type _key_.
 * These are:
   -- constants: int, word, char, string (with various precisions for int and word)
      (key constructors I, W, C, S)
   -- data constructors: these can be either constant or non-constant
      (key constructor D)
   -- vector length (key constructor V)
 * To determine the variety of a given OR node, look at the key of its first variant.
 * The treatment of discrimination over vector lengths will be reduced to a switch over
   the int value of the length.
 *)

(* andKind: two flavors of AND nodes, one for record/tuples, and one for vector elements
 *  the andKind determines the selection operator for extracting elements *)
datatype andKind
  = RECORD
  | VECTOR

type AOinfo =
     {id : int,              (* unique identity of the node, for efficient maps on nodes *)
      typ : T.ty,            (* type of a value matching at the node *)
      path : path,           (* path to this node; serves as the node name & secondary unique identifier *)
      vars : varBindings,    (* primary variable bindings at the node, cause rule defaultings *)
      asvars : varBindings}  (* secondary (as) variable bindings at the node, no defaulting *)

datatype andor
  = AND of   (* product patterns and contents of vectors *)
    {info : AOinfo,
     andKind : andKind,      (* a record/tuple, or a vector [could split AND constr instead] *)
     live : LS.set,        (* live rules: rules with product pats or variagbles at this pattern
				point. Should contain all layers, so does not seem necessary.
				Drop live field for AND? *)
     children : andor list}  (* tuple components as children -- AND node *)

  | OR of (* datatype, vector, or constant pattern/type *)
    {info : AOinfo,
     live : LS.set,        (* layers matching explicitly or because of variable defaulting *)
     variants: variants} (* the branches/choices of the OR node; always non-null *)

  | SINGLE of  (* singular datacon (const or applied); a kind of no-op for pattern matching,
		* but it needs to be destructed if not constant *)
    {info : AOinfo,
     variant: variant}   (* key (the dcon) and its arg andor, LEAF if constant *)

  | VARS of  (* a node occupied only by variables;
              * VIRTUAL field : live = map #2 vars = rules having _a_ variable at this point *)
    {info : AOinfo,
     live: LS.set}     (* layers matching here by default; needed ??? *)

  | LEAF of   (* used as the andor of variants with constant keys, with live layers
               * A LEAF node does not have an independent type; its type is determined
	       * by the parent OR node's AOinfo. *)
    {path: path,         (* path is the parent path extended by key *)
     live: LS.set}     (* layers having _this_ key (= last of path) at this pattern point *)

  | INITIAL   (* initial empty andor into which initial pattern is merged
               * to begin the construction of an AND-OR tree *)

withtype variant = K.key * andor
and variants = andor Variants.variants
(* this pushes the discrimination of the OR-kind into the keys of the variants. *)

(* potentially useful functions:

eqNode : andor * andor -> bool
(two nodes are equal if their path component is equal, needed only for OR nodes?)

followPath : path * andor -> andor
(the andor subtree located at path in the given andor tree)

pathToType : path * ty -> ty  (* don't need a node, path suffices *)

andorBreadth : andor -> int option
(number of children of an OR node, NONE for non-OR nodes; == SOME(length variants) )

*)

(* getInfo : andor -> AOinfo *)
fun getInfo(AND{info,...}) = info
  | getInfo(OR{info,...}) = info
  | getInfo(SINGLE{info,...}) = info
  | getInfo(VARS{info,...}) = info
  | getInfo(LEAF{path,...}) = bug "getInfo(LEAF)"
  | getInfo INITIAL = bug "getInfo(INITIAL)"

(* getId : andor -> int *)
(* fails (bug) for andor nodes without info: LEAF, INITIAL *)
fun getId andor = #id (getInfo andor)

(* getType : andor -> T.ty *)
(* fails (bug) for andor nodes without info: LEAF, INITIAL *)
fun getType andor = #typ (getInfo andor)

(* getPath : andor -> path *)
fun getPath (LEAF{path,...}) = path
  | getPath INITIAL = bug "getPath:INITIAL"
  | getPath andor = #path (getInfo andor)  (* otherwise, andor has info *)

(* getLive : andor -> LS.set
   live layers at a node *)
fun getLive(AND{live,...}) = live
  | getLive(OR{live,...}) = live
  | getLive(SINGLE{variant,...}) = getLive (#2 variant)
  | getLive(VARS{live,...}) = live
  | getLive(LEAF{live,...}) = live
  | getLive INITIAL = bug "getLive(INITIAL)"


(* findKey : key * variant list -> andor option *)
(* search for a variant with the given key *)
fun findKey (key, (key',node)::rest) =
    if K.eqKey(key,key') then SOME node
    else findKey(key, rest)
  | findKey (_, nil) = NONE

(* FIX: getNode and parentNode not used.  Therefor commented out.

(* getNode : andor * path * int -> andor *)
(* REQUIRE: for getNode(andor,path,depth): depth <= length path *)
fun getNode(andor, _, 0) = andor
  | getNode(andor, nil, _) = andor
  | getNode(andor, key::path, depth) =
    (case (andor,key)
      of (AND{children,...}, K.R i) =>
	   getNode(List.nth(children, i),path,depth-1)
       | (OR{variants,...},key) =>
	   (case findKey(key,variants)
	      of NONE => bug "getNode"
	       | SOME node => getNode(node, path, depth-1))
       | (SINGLE{variant,...}, key) =>
	   getNode(#2 variant, path, depth-1)
       | ((VARS _ | LEAF _),_) => bug "getNode(VARS|LEAF)"
       | _ => bug "getNode arg")

(* parentNode: andor * andor -> andor *)
(* never called? *)
fun parent (andor, rootAndor) =
    let val path = getPath(andor)
        val d = length(path) - 1
     in getNode(rootAndor, path, d)
    end
*)

(* ---------------------------------------------------------------------- *)
(* decision trees *)

datatype decTree
  = DLEAF of L.layer * trace
      (* bind variables consistent with layer and dispatch to RHS for layer's ruleno *)
  | DMATCH of trace (* trace of decision ponts in leading to this DMATCH node *)
      (* generate a match exception.
       * would be redundant if we were adding a final default rule with wildcard pat
       * to guarantee that all pattern sets are known to be exhaustive,
       * but the trace argument should allow us to construct a counterexample *)
  | CHOICE of
    {node : andor,  (* an OR node used for dispatching *)
     choices : decVariants,  (* corresponding to the (OR) node variants *)
     default : decTree option}
       (* + a default if node is partial and there are defaults (vars on the path),
        * BUT, if the node is partial and there are no natural defaults from
        * variables, a default producing MATCH will be supplied. So the default
        * will always be SOME dt unless it is not needed because the choices are
        * exhaustive. If the default leads to a MATCH, it will be SOME DMATCH *)
withtype decVariants = decTree Variants.variants

fun getNode (CHOICE {node, ...}) = node
  | getNode _ = bug "getNode"
		    
end (* local *)
end (* structure MCTypes *)
