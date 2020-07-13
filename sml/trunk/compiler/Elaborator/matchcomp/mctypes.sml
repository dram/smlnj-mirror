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
  structure V = Var
  structure SV = SVar
  open Absyn
  (* also used/mentioned: IntConst, ListPair *)
in

fun bug msg = EM.impossible ("MCTypes: " ^ msg)

type rule = Absyn.pat * Absyn.exp
type ruleno = R.ruleno    (* == int, the index number of a rule in the match, zero-based *)
type ruleset = R.ruleset  (* == IntBinarySet.set *)
   (* a set of rule numbers, maintained in strictly ascending order without duplicates *)

type binding = Var.var * ruleno
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
  = D of T.datacon        (* datacon key, possibly constant *)
  | V of int              (* vector length; ASSERT int >= 0 *)

  (* following constant keys supercede previous constCon constructors *)
  | I of T.ty IntConst.t  (* int constant, as determined by ty *)
  | W of T.ty IntConst.t  (* word constant, as determined by ty *)
  | C of char             (* char constant (span depends of charwidth) *)
  | S of string           (* string constant *)

  (* record selection: not a choice discriminator, but a selection key for products,
   * Will only appear in paths, never in variants. ASSERT: int >= 0 *)
  | R of int

(* eqKey : key * key -> bool
 * type info disregarded when comparing Dkeys and Vkeys *)
fun eqKey (D dcon1, D dcon2) = TU.dataconEq(dcon1,dcon2)
  | eqKey (V l1, V l2) = l1 = l2
  | eqKey (I c1, I c2) = IntConst.same(c1,c2)
  | eqKey (W c1, W c2) = IntConst.same(c1,c2)
  | eqKey (C c1, C c2) = c1 = c2  (* character keys *)
  | eqKey (S s1, S s2) = s1 = s2
  | eqKey (R i1, R i2) = i1 = i2
  | eqKey _ = false  (* mismatching key constructors *)

fun keyToString (D dcon) = substring((TU.dataconName dcon),0,1)
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

fun pathToString path =
    concat("<"::(map keyToString path@[">"]))

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

datatype andor
  = AND of   (* product pattern *)
    {svar: SV.svar,            (* svar to be bound to value at this point, contains type *)
     path : path,              (* unique path to this node *)
     asvars: asBindings,       (* layered variables bound at _this_ point *)
     vars : varBindings,       (* variables bound at this point *)
     direct : ruleset,         (* direct rules: rules with product pats at this pattern point *)
     defaults : ruleset,       (* rules matching here because of variables along the path *)
     children: andor list}     (* tuple components as children -- AND node *)
  | OR of (* datatype, vector, or constant pattern/type *)
    {svar: SV.svar,
     path : path,              (* ditto *)
     asvars: asBindings,       (* ditto *)
     vars : varBindings,       (* ditto *)
     direct : ruleset,         (* rules matching one of the variant keys at this point *)
     defaults: ruleset,        (* ditto *)
     variants: variant list}   (* the branches/choices of OR node; non-null *)
  | SINGLE of  (* singular datacon app, a kind of no-op for pattern matching *)
    {svar : SV.svar,
     path : path,              (* ditto *)
     asvars: asBindings,       (* ditto *)
     vars: varBindings,        (* ditto *)
     dcon: T.datacon,          (* the singleton dcon of the datatype for this node *)
     arg: andor}               (* arg of the dcon, LEAF if it is a constant *)
  | VARS of  (* a node occupied only by variables;
              * VIRTUAL field : direct = map #2 vars = rules havine _a_ variable at this point *)
    {svar: SV.svar,
     path : path,              (* ditto *)
     asvars: asBindings,       (* ditto *)
     vars: varBindings,
     defaults: ruleset}        (* rules matching here by default *)
  | LEAF of   (* used as the andor of variants with constant keys, with direct and default rules
               * but no svar, since the svar is bound at the parent OR node. A LEAF
	       * node also does not have an independent type; its type is determined
	       * by the parent OR node (through its svar). *)
    {path: path,               (* path is parent path extended by key *)
     direct: ruleset,          (* rules having _this_ key (end of path) at this point *)
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

(* getPath : andor -> path *)
fun getPath(AND{path,...}) = path
  | getPath(OR{path,...}) = path
  | getPath(SINGLE{path,...}) = path
  | getPath(VARS{path,...}) = path
  | getPath(LEAF{path,...}) = path
  | getPath INITIAL = bug "getPath(INITIAL)"

(* getSvar : andor -> SV.svar *)
fun getSvar(AND{svar,...}) = svar
  | getSvar(OR{svar,...}) = svar
  | getSvar(SINGLE{svar,...}) = svar
  | getSvar(VARS{svar,...}) = svar
  | getSvar(LEAF _) = bug "getSvar(LEAF)"
  | getSvar INITIAL = bug "getSvar(INITIAL)"

(* getType : andor -> T.ty *)
(* fails (bug) for andor nodes without svar: LEAF, INITIAL *)
fun getType andor = SV.svarType (getSvar andor)

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
(* search for a variant with the given key *)
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
  | DMATCH (* of trail -- path in decTree to this DMATCH node *)
      (* would be redundant if we add a final default rule with wildcard pat
       * to guarantee that all pattern sets are known to be exhaustive,
       * but the trail argument should give an easy way to construct a counterexample *)
  | CHOICE of
    {node : andor,  (* an OR node used for dispatching *)
     choices : decVariant list,  (* corresponding to the (OR) node variants *)
     default : decTree option}
       (* + a default if node is partial and there are defaults (vars on the path) *)
withtype decVariant = key * decTree


(* match "code" *)
(* "bodies" of expressions for rule RHSs will be Absyn.exp,
 * representing a "raw" RHS of a rule. *)
datatype mcexp
  = Var of SV.svar  (* how/where used? *)
      (* "sv" as an expression *)
  | Letr of SV.svar list * SV.svar * mcexp
      (* "let (sv1, ..., svn) = sv0 in body"; destructure an AND *)
  | Letf of SV.svar * mcexp * mcexp
      (* "let f = << fn sv => fbody >> in body" *)
      (* 1st mcexp will always be an Sfun. The function will be a
       * functionalized RHS. *)
  | Letm of V.var list * SV.svar list * Absyn.exp
      (* "let (v1, ..., vn) = (sv1, ..., svn) in rhsexp" *)
      (* non-functionalized, single use, rule RHS, with linkage for svars *)
  | Case1 of SV.svar * T.datacon * SV.svar * mcexp
      (* "let dcon sv2 = sv1 in body" == "let sv2 = dcon^{-1} sv1 in body" *)
      (* Destructure of a SINGLE datacon; not generated if dcon is constant. *)
  | Case of SV.svar * (key * SV.svar option * mcexp) list * mcexp option
      (* Destructure an OR, with svar binding if key is not a constant. *)
  | Sfun of V.var list * Absyn.exp
      (* "fn (v0, ..., vn) => body"; functionalized, multi-use rule RHS *)
  | Sapp of SV.svar * SV.svar list
      (* "sv0 (sv1, ..., svn)";  A-normal-style: function and args have all been
       * bound to svars. Will only be used for instances of functionaolized RHSs,
       * so sv0 will be bound (using Letf) to a RHS function. *)
  | Tfun of T.typevar list * mcexp
      (* "tfun (tyv0, ..., tyvn) => body"; type function with tyvar parameters. *)
      (* ??? type-level abstraction may be left to Translate phase? *)
  | MATCH
      (* "raise MATCH"; May be redundant if matches guaranteed exhaustive. *)

(* NOTE: we don't need letm case if we translate svars to corresponding vars while
 * translating the body of the letm (using map from svar to (var,rule)), and keeping
 * track of which rule rhs we are translating. *)

end (* local *)
end (* structure MCTypes *)
