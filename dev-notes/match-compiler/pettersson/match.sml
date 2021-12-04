(* pet/mc.sml *)
(* a prototype match compiler based on Pettersson, Chapter 7 *)

(* modified to using tupling instead of currying style *)

structure MC =
struct

local
  open Dcons Absyn
  type dcon = Dcons.dcon

  (* snoc : 'a list * 'a -> 'a list
   *  "reverse cons", adds x at the end of xs *)
  fun snoc (xs, x) = xs @ [x]

in
(* let constants be "represented" by nullary constructor applications,
 * e.g. true ==> APP ("TRUE", TUP nil), as in Augustssen/Wadler *)

(* ppat: preprocessed (internal) patterns, Pettersson's Pat;
   All variables are replaced by PPVAR, so variable names are dropped. *)
datatype ppat
  = PPCON of dcon * ppat
  | PPTUP of ppat list
  | PPVAR  (* Pettersson's WILD, printed as "?" *)

(* type occ = var * nat list  (* "occurrences", paths rooted at a variable *) *)

datatype link
  = CL of dcon     (* dcon choice link *)
  | PL of int      (* tuple projection link *)

(* paths are rooted at the top node, designated by the empty path [].
 * Can think of the path as being relative to a single original root match
 * variable corresponding to the root of the pattern space that is matched
 * to the whole argument value. *)
type path = link list
type ruleno = int

(* pmatrix: preprocessed pattern matrices *)
type prow = ppat list  (* row of patterns *)
type pcol = ppat list  (* column of patterns *)
type pmatrix  = ppat list list (* pattern matrix, ambiguous order *)
type pmatrixR = prow list  (* "row-major" pattern matrix = pmatrix *)
type pmatrixC = pcol list (* "column-major" pattern matrix = pmatrix *)
type indicesR = int list  (* row indices in ascending order *)

(* INVARIANT (for pmatrix, pmatrixR, pmatrixC: all elements have same length
 * Note that pmatrix == pmatrixR == pmatrixC *)

(* "augmented pmatrix with paths and rules
 * INVARIANT: if |pmatrix| = m x n, then |paths| = n and |rules| = m *)
type amatrix = {pmatrix : pmatrixR, paths: path list, rules: ruleno list}

(* dt is the datatype of the "decision tree" (called "state" in [2]),
 * the content of the dt should be sufficient to generate the match
 * expression (internal variable bindings and accesses, etc.)
 * [May need to be augmented to implement "merging of equivalent nodes".] *)
datatype dt (* "state" *)
  = TEST of path * (dcon * dt) list * dt option  (* "Test" state; choice node *)
  | LEAF of ruleno                        (* "Final" state; leaf node *)


(* pattern preprocessing *****************************************************)

(* 1. for each source rule (pat, exp) [plus root match variable "x"],
      (a) build an environment mapping source vars inthe pat to paths
      (b) each path could be translated into a unique "match" variable, so
          this environment can be treated as a variable substitution s
          This is well-defined because each source variable appears at
	  most once in a pattern.
      (c) the "final" state is (exp, s), or s(exp)
 *)

(* svenv : var --> path environment (alist representation) *)
type svenv = (var * path) list
fun svenvBind (var, path, svenv) = (var, path) :: svenv

(* preprocess : pat -> ppat * svenv
 * uses the fact that any var appears at most once in a pattern *)
fun preprocess (pat: pat): ppat * svenv =
    let fun proc (path, pat, svenv) =
	    (case pat
	      of PVAR v => (PPVAR,  svenvBind (v, path, svenv))
	       | PCON (dcon, argpat) =>
		 let val (argppat, svenv') = proc (snoc (path, CL dcon), argpat, svenv)
		  in (PPCON (dcon, argppat), svenv')
		 end
	       | PTUP pats =>
		 let fun folder (i, pat, (pats, svenv0)) =
			 let val (ppat, svenv1) =
				 proc (snoc (path, PL i), pat, svenv0)
			  in (ppat :: pats, svenv1 @ svenv)
			 end
		     val (ppats', svenv'') =
			 List.foldli folder (nil, svenv) pats
		 in (PPTUP (rev ppats'), svenv'')
		 end)
    in proc (nil, pat, nil)
    end


(* some utility functions ****************************************************)

(* iselect : 'a list * int list -> 'a list *)
(* REQUIRE: is is an ascending list of indices i s.t. 0 <= i < length xs *)
fun iselect (xs, is) =
    let fun sel (xs as (x::xs'), n, indices as (i::is')) =
	    if i = n then x :: sel (xs', n+1, is')
	    else sel (xs', n+1, indices)
     in sel (xs, 0, is)
    end

(* mergeIndices : int list * int list -> int list
 * REQUIRE: is and js are strictly ascending index lists;
 * RESULT: ascending index list conltaining the union of is and js (with no dups) *)
fun mergeIndices (nil, js) = js
  | mergeIndices (is, nil) = is
  | mergeIndices (is as i::is', js as j::js') =
     (case Int.compare (i,j)
       of EQUAL => i :: mergeIndices (is', js')
	| LESS => i :: mergeIndices (is', js)
	| GREATER => j :: mergeIndices(is, js'))


(* transpose : pmatrix -> pmatrix
 *  REQUIRE: all "rows" (elements) of pmatrix have the same length
 *  simple, "brute force" version of list-matrix transpose  *)
fun transpose (m: pmatrix) : pmatrix =
    List.tabulate (length (hd m),  (* number of columns *)
		   fn n => map (fn row => List.nth(row,n)) m)

(* destructTuple : pcol -> pmatrixC
 *  REQUIRE: arg is a pcol starting with a PPTUP pattern;
 *  all patterns in pcol will be either PPVAR or PPTUP *)
fun destructTuple (PPTUP pats :: rest) =
    let val varRow = List.tabulate (length pats, (fn _ => PPVAR))
	fun mkrow (PPTUP pats') = pats'
	  | mkrow PPVAR = varRow
    in pats :: map mkrow rest
    end


(* The Match Compiler **********************************************************)

(* columnScan: path list * pmatrixC -> (path * pcol) option * path list * pmatrixC
    iterate over columns of the pmatrix (in column-major form), looking for a PCON
    column, destructing PTUP columns in place along the way. Returns the PCON column
    c and its path, if found, and the pmatrixT\c, and corresponding modified path list.
    If this gets through all columns without finding a PCON column, then
    all the resulting columns are headed by PVAR, because PTUP columns would have
    been recursively flattened.
    If we encounter a PCON column, all PTUP columns before that PCON column will have
    been flattened, but none of any PTUP columns after the PCON column will be flattened. *)
fun columnScan (paths, columns) =
    let fun scan (prevpaths, prevcols, curpath::restpaths, curcol::restcols) =
	    (case hd curcol
	      of PPCON (dcon, argpat) =>  (* found a PPCON column *)
		 (SOME (curpath, curcol), revAppend (prevpaths, restpaths),
		  revAppend (prevcols, restcols))
	       | PPTUP pats =>  (* found a PPTUP column, flatten it in place and continue *)
		 scan (prevpaths, prevcols,
		       (List.mapi (fn (i,p) => snoc(curpath, PL i)) pats) @ restpaths,
		       (destructTuple curcol) @ restcols)
	       | PPVAR => (* found a PPVAR column, leave it in place and continue *)
		 scan (snoc (prevpaths, curpath), snoc (prevcols, curcol),
		       restpaths, restcols))
	  | scan (prevpaths, prevcols, nil, nil) = (NONE, prevpaths, prevcols)
     in scan (nil, nil, paths, columns)
    end

(* columnSplit : pcol -> (dcon * indices) list * indices
 *  REQUIRES : pcol is a PCON column (=> contains only PCON and PVAR ppats)
 *  Case split on a PCON column, returning an alist mapping present dcons to their
 *  row indices plus the row indices of PVARS in the column. The row indices for
 *  a dcon also include all the var indices. *)
fun columnSplit pcol =
    let fun insert (dcon, i, nil) = [(dcon, [i])]
	  | insert (dcon, i, (binder as (dcon', indices))::rest) =
	      if dcon' = dcon
	      then (dcon', i::indices) :: rest
	      else binder :: insert (dcon, i, rest)
	fun scan (PPCON (dcon, _)::rest, n, dcontable, vindices) =
	      scan (rest, n+1, insert (dcon, n, dcontable), vindices)
	  | scan (PPVAR::rest, n, dcontable, vindices) =
	      scan (rest, n+1, dcontable, n::vindices)
	  | scan (nil, _, dcontable, vindices) = (dcontable, vindices)
        val (dcontable, vindices) =  scan (pcol, 0, nil, nil)
     in (map (fn (dcon, indices) => (dcon, rev indices)) dcontable,
	 rev vindices)
    end

(* branch : pcol * path * amatrix -> dt
 *  If the dcons in a PCON column are not exhaustive (e.g. cons without nil)
 *  and there are no variable patterns (PVAR) in the column, then there should
 *  be a MATCH_ERROR default branch.
 *  ASSERT: path not in paths *)
fun branch (pcol, path, {pmatrix = rows, paths, rules}: amatrix) : dt =
    let val (dcon_rows, var_rows) : (dcon * indicesR) list * indicesR =
	    columnSplit pcol
        val dcons_present = map #1 dcon_rows
	fun mkDconDT (dcon, indicesDcon) : (dcon * dt) =
 	    let val constDcon = isConstant dcon
		val indices = mergeIndices (indicesDcon, var_rows)
		fun residualRow i =
		    let val thisrow = List.nth (rows, i)
			val columnPat = List.nth (pcol, i)
		     in if constDcon
		        then thisrow (* no argpat, rows unchanged *)
		        else (case columnPat
			       of PPCON (_, argpat) =>
				  argpat :: thisrow  (* add argpat to row *)
				| PPVAR => PPVAR :: thisrow)
		    end
	        val residualPmatrix = map residualRow indices
		val residualPaths = if constDcon then paths else snoc (path, CL dcon) :: paths
		val residualRules = iselect (rules, indices)
		val dt = match {pmatrix = residualPmatrix, paths = residualPaths,
				rules = residualRules}
	     in (dcon, dt)
	    end
	val dcon_dts = map mkDconDT dcon_rows
	val default_dt =
	    if isExhaustive dcons_present
	    then NONE
	    else SOME (match {pmatrix = iselect (rows, var_rows), paths = paths,
			      rules = iselect (rules, var_rows)})
     in TEST (path, dcon_dts, default_dt)
    end

(* match : amatrix -> dt *)
and match ({pmatrix, paths, rules} : amatrix) =
    let val (pathColOp, paths', pmatrixC') = columnScan(paths, transpose pmatrix)
 	  (* scan for PCON column *)
     in case pathColOp
	  of NONE => LEAF (hd rules)
	       (* no PPCON columns found ==> all PPVARS, Variable Rule applies *)
	   | SOME (path, pcol) =>
	       let val amatrix = {pmatrix = transpose pmatrixC', paths = paths', rules = rules}
	        in branch (pcol, path, amatrix)
	       end
    end

end (* top local *)
end (* structure MC *)

(* TODO items:

 * "optimization" or "state merging" for decision tree
 *  this may be necessary to avoice retesting the same value node (same path)
 *  multiple times.
 *
 * generating "code", i.e. absyn expression for match, from decision tree
 *   -- need match variables bound to any non-constant dcon destructs
 *      source pattern variables defined in terms of these match variables
 *   -- detect rules with multiple dispatches and abstract them as functions
 *)
