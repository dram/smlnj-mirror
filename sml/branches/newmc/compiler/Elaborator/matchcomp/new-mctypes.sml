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
  open Absyn
  (* also used/mentioned: IntConst, ListPair *)
in

fun bug msg = EM.impossible ("MCTypes: " ^ msg)

type rule = Absyn.pat * Absyn.exp
type ruleno = R.ruleno    (* == int, the index number of a rule in the match, zero-based *)
type ruleset = R.ruleset  (* == IntBinarySet.set *)
   (* a set of rule numbers (no duplicates) *)

(* layers -- refinement of (rule) layering *)

(* type layer -- a generalization of ruleno that accounts for OR-pattern
 * the integers in the int list will be 0 or 1, designating left or right,
 * thus the int list is a bit string representing left or right branches of
 * nested "OR patterns". Layers are ordered, with the ruleno dominating the
 * bitstring.
 * INVARIANT: a pattern variable can "occur" only once along any orpath
 * INVARIANT: a pattern variable can occur multiple times on "incomparable"
 *   orpaths
 *)
type layer = ruleno * int list  (* (rule, orpath) *)

fun layerEq ((r1,s1): layer, (r2,s2)) =
    r1 = r2 andalso ListPair.allEq (op =) (s1, s2)

(* two occurrences of a variable in a pattern must be separated by an OR, so
 * their layer paths must _differ_ at some point *)
fun layerPathLess (nil, nil) = false
  | layerPathLess (nil, _) = false
  | layerPathLess (_, nil) = false
  | layerPathLess (b1::rest1, b2::rest2) =
    b1 < b2 orelse b1 = b2 andalse layerPathLess (rest1,rest2)

(* WRONG
fun layerLE ((r1,s1): layer, (r2,s2)) =
    r1 <= r2 orelse
    r1 = r2 andalso layerPathLess (s1,s2)
*)

fun layerLT ((r1,s1): layer, (r2,s2)) =
    r1 < r2 orelse
    r1 = r2 andalso layerPathLess (s1,s2)

fun layerLeft (r,s) = (r, s@[0])
fun layerRight (r,s) = (r, s@[1])

type binding = V.var * layer
   (* a variable bound at some point in the given rule, either as a
    * basic var pattern (VARpat) or through an "as" pattern (LAYEREDpat) *)
type varBindings = binding list  (* variables bound by VARpat *)
type asBindings = binding list   (* variables bound by LAYEREDpat, i.e. an "as" pattern *)

(* keys: most keys (D,V,I,W,C,S) are used to discriminate choices in the "variants"
 *   field of OR nodes and the decVariants of decision trees. These key values
 *   (appearing in variants) determine the different flavors of OR nodes
 *   (data, vector length, and 4 varieties of constants).
 *   There is an extra R (for record) key representing record/product selection.
 *   R keys appear only in paths to indicate product projections. *)
datatype key
  = D of T.datacon * T.tyvar list
     (* datacon key, possibly constant, with instantiation tyvars *)
  | V of int              (* vector length; ASSERT int >= 0 *)

  (* following constant keys supercede previous constCon constructors *)
  | I of T.ty IntConst.t  (* int constant, as determined by ty *)
  | W of T.ty IntConst.t  (* word constant, as determined by ty *)
  | C of char             (* char constant (span depends on charwidth) *)
  | S of string           (* string constant *)

  (* record selection: not a choice discriminator, but a selection key for products,
   * Will only appear in paths, never in variants. ASSERT: int >= 0 *)
  | R of int

(* eqKey : key * key -> bool
 * type info disregarded when comparing Dkeys and Vkeys *)
fun eqKey (D (dcon1,_), D (dcon2,_)) = TU.eqDatacon(dcon1,dcon2)
      (* we can ignore type instantiation tyvars, which are guaranteed to be 
       * equivalent at the same pattern node. *)
  | eqKey (V l1, V l2) = l1 = l2
  | eqKey (I c1, I c2) = IntConst.same(c1,c2)
  | eqKey (W c1, W c2) = IntConst.same(c1,c2)
  | eqKey (C c1, C c2) = c1 = c2  (* character keys *)
  | eqKey (S s1, S s2) = s1 = s2
  | eqKey (R i1, R i2) = i1 = i2
  | eqKey _ = false  (* mismatching key constructors *)

fun keyToString (D (dcon,_)) = Symbol.name(TU.dataconName dcon)
  | keyToString (V n) = "V"^(Int.toString n)
  | keyToString (I{ival,ty}) = "I"^(IntInf.toString ival)
  | keyToString (W{ival,ty}) = "W"^(IntInf.toString ival)
  | keyToString (C c) = "C"^(Char.toString c)
  | keyToString (S s) = "S["^ s ^ "]"
  | keyToString (R i) = Int.toString i

(* ================================================================================ *)
(* paths:
   paths locate points in the "pattern space" determined by a sequence of patterns
      (a finite subtree of the complete pattern tree determined by the common type of
      the patterns). Points in the pattern space correspond to andor nodes, which
      therefore have a unique identifying path.
*)

(* a path is well formed only if its links are well formed *)
type path  = key list (* keys ordered from root to node *)
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

fun pathToString path =
    let fun ts [] = []
	  | ts [s] = [s]
	  | ts (s::ss) = s :: "." :: ts ss
     in concat("<":: ts (map keyToString path) @ [">"])
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
 *   Do we need defaults fields for VARS and LEAF nodes?  Yes (probably).
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
     {id : int,              (* unique identity of node, for efficient maps on nodes *)
      typ : T.ty,            (* type of node value *)
      path : path,           (* path to this node; serves as node name, secondary unique identifier *)
      vars : varBindings,    (* primary variable bindings at node *)
      asvars : varBindings}  (* secondary (as) variable bindings at node *)

datatype andor
  = AND of   (* product patterns and contents of vectors *)
    {info : AOinfo,
     andKind : andKind,        (* a record/tuple, or a vector; could split AND constr instead *)
     direct : ruleset,         (* direct rules: rules with product pats at this pattern point; needed? *)
     defaults : ruleset,       (* rules matching here because of (primary) variables along the path *)
     children : andor list}    (* tuple components as children -- AND node *)

  | OR of (* datatype, vector, or constant pattern/type *)
    {info : AOinfo,
     direct : ruleset,         (* rules matching one of the variant keys at this point *)
     defaults: ruleset,        (* ditto *)
     variants: variant list}   (* the branches/choices of the OR node; always non-null *)

  | SINGLE of  (* singular datacon (const or applied); a kind of no-op for pattern matching,
		* but it needs to be destructed if not constant *)
    {info : AOinfo,
     variant: variant}         (* key (the dcon) and its arg andor, LEAF if constant *)

  | VARS of  (* a node occupied only by variables;
              * VIRTUAL field : direct = map #2 vars = rules having _a_ variable at this point *)
    {info : AOinfo,
     defaults: ruleset}        (* rules matching here by default *)

  | LEAF of   (* used as the andor of variants with constant keys, with direct and default rules
               * but no svar, since the svar is bound at the parent OR node. A LEAF
	       * node also does not have an independent type; its type is determined
	       * by the parent OR node (through its svar). *)
    {path: path,               (* path is parent path extended by key *)
     direct: ruleset,          (* rules having _this_ key (= last of path) at this pattern point *)
     defaults: ruleset}

  | INITIAL   (* initial empty andor into which initial pattern is merged
               * to begin the construction of an AND-OR tree *)

withtype variant = key * andor
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

(* getDirect : andor -> ruleset *)
fun getDirect(AND{direct,...}) = direct
  | getDirect(OR{direct,...}) = direct
  | getDirect(SINGLE{variant,...}) = getDirect (#2 variant)
  | getDirect(VARS{info, ...}) = R.fromList(map #2 (#vars info))
  | getDirect(LEAF{direct,...}) = direct
  | getDirect INITIAL = bug "getDirect(INITIAL)"

(* getDefaults : andor -> ruleset *)
fun getDefaults(AND{defaults,...}) = defaults
  | getDefaults(OR{defaults,...}) = defaults
  | getDefaults(SINGLE{variant,...}) = getDefaults (#2 variant)
  | getDefaults(VARS{defaults,...}) = defaults
  | getDefaults(LEAF{defaults,...}) = defaults
  | getDefaults INITIAL = bug "getDefaults(INITIAL)"

(* getLive : andor -> ruleset
   live rules is union of direct and defaults *)
fun getLive(AND{direct,defaults,...}) = R.union(direct,defaults)
  | getLive(OR{direct,defaults,...}) = R.union(direct,defaults)
  | getLive(SINGLE{variant,...}) = getLive (#2 variant)
  | getLive(VARS{defaults,...}) = defaults  (* direct subset defaults *)
  | getLive(LEAF{direct,defaults,...}) = R.union(direct,defaults)
  | getLive INITIAL = bug "getLive(INITIAL)"


(* findKey : key * variant list -> andor option *)
(* search for a variant with the given key *)
fun findKey (key, (key',node)::rest) =
    if eqKey(key,key') then SOME node
    else findKey(key, rest)
  | findKey (_, nil) = NONE

(* FIX: getNode and parentNode not used *)

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
       | (SINGLE{variant,...}, key) =>
	   getNode(#2 variant, path, depth-1)
       | ((VARS _ | LEAF _),_) => bug "getNode(VARS|LEAF)"
       | _ => bug "getNode arg")

(* parentNode: andor * andor -> andor *)
fun parent (andor, rootAndor) =
    let val path = getPath(andor)
        val d = length(path) - 1
     in getNode(rootAndor, path, d)
    end


(* ---------------------------------------------------------------------- *)
(* decision trees *)

datatype decTree
  = DLEAF of ruleno * trace
      (* bind variables consistent with trace and dispatch
       * to RHS(ruleno) *)
  | DMATCH of trace (* trace of decision ponts in leading to this DMATCH node *)
      (* generate a match exception.
       * would be redundant if we were adding a final default rule with wildcard pat
       * to guarantee that all pattern sets are known to be exhaustive,
       * but the trace argument should allow us to construct a counterexample *)
  | CHOICE of
    {node : andor,  (* an OR node used for dispatching *)
     choices : decVariant list,  (* corresponding to the (OR) node variants *)
     default : decTree option}
       (* + a default if node is partial and there are defaults (vars on the path),
        * BUT, if the node is partial and there are no natural defaults from
        * variables, a default producing MATCH will be supplied. So the default
        * will always be SOME dt unless it is not needed because the choices are
        * exhaustive. If the default leads to a MATCH, it will be SOME DMATCH *)
withtype decVariant = key * decTree

fun getNode (CHOICE {node, ...}) = node
  | getNode _ = bug "getNode"
		    
end (* local *)
end (* structure MCTypes *)
