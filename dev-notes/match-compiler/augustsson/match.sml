(* augustsson/match.sml *)
(* Match compilation following Wadler's presentation in Chapter 5 of
 * "Implementation of Functional Languages", 1987? *)

structure MC =
struct

local
  open Absyn
in

(************************* the match compiler ***************************************)

(* equation: a rule of a match, where the original lhs pattern is in the process of
 * being "destructed" into a series of patterns corresponding to a series of subtrees
 * of the original match target. *)
type equation = pat list * exp

(* matchArg : the argument tuple for the match function, the input for building
 * the match expression.
 * INVARIANT: length(var list) = length (#1 eqn) for each eqn in equation list
 * (vars, eqns, default):
 *  -- vars are bound to a list of "targets", or values being matched. It has one 
 *     element to begin with, representing the original subject of the match
 *  -- eqns are the match "rules", where there is a pattern for each target
 *     variable in vars (so |#1 eqn| = |vars| for each eqn).
 *  -- default is the default expression to be used in case of failure to match
 *     any of the equations *)
type matchArg = var list * equation list * exp

(* fstPat : equation list -> pat
 * REQUIRES: equation list non-null *)
fun fstPat ((pat::_, _) :: _) = pat

(* getCon : equation -> dcon *)
(* if the first pattern of the equation is a construction, returns the constructor,
 *   otherwise the result is a MATCH exception *)
fun getCon (PCON (c, _) :: _, _) = c

(* variables: expression variables and match admin variables
   variables are represented by strings, but it is convenient to separate
   variables into two disjoint classes:

   (1) variables occurring in original source expressions; and
   (2) "administrative" variables introduced by the match compilation algorithm.

   We use the convention that expression variables are of the form v<num>
   and match admin variables are of the form u<num>. So they are distinguished
   by their initial letter (v or u). This makes substituion of match variables
   into expressions simpler since they cannot be captured or conflict with 
   expression variable bindings. Also, we assume all bound match variables are
   unique.
*)

(* intToVar, intToMVar : int -> var *)
(* int to variable (i.e. string) *)
fun intToVar n = "v" ^ Int.toString n
fun intToMVar n = "u" ^ Int.toString n

(* newMVar : () -> var *)
(* generate "fresh" match "admin" variables, using a counter *)
local
  val mvcount = ref 5
  fun next () = !mvcount before (mvcount := !mvcount + 1)
in
  fun newMVar () = intToMVar (next ())
end

fun tack (x, xs::xss) = (x::xs) :: xss

datatype patKind = PKvar | PKcon | PKtup

(* eqnPatKind : equation -> patKind *)
fun eqnPatKind ((pat::_, _): equation): patKind =
    case pat
     of PVAR _  => PKvar  (* variable pattern *)
      | PCON _  => PKcon  (* constructor pattern *)
      | PTUP _  => PKtup  (* tuple pattern *)
		       
(* partitionEqns : equation list -> equation list list *)
(* partitions the list of equations into equation "blocks" where all the
 * equations in a block have the same kind of initial pattern (var, con, tuple).
 * For a given equation list, there may be mixture var blocks and con blocks,
 * or var blocks and tuple blocks, but not a mixture of con blocks and tuple blocks
 * (because of type checking). 
 * Order of "same-constructor" equations in a block is preserved from the original
 * equation ordering, so equation priorities are preserved. Note that priority 
 * ordering between equations with different lead constructors is not relevant,
 * because they do not "compete".
 * Order of equations accross blocks is also preserved. *)
fun partitionEqns nil = nil
  | partitionEqns [x] = [[x]]
  | partitionEqns (x::y::xs) =
    if eqnPatKind x = eqnPatKind y then tack (x, partitionEqns (y::xs))
    else [x] :: partitionEqns (y::xs)

(* choose : dcon * equation list -> equation list *)
(* returns sublist of eqns whose dcon equals c, assuming all eqns are
 * isCon *)
fun choose (c, eqns) = List.filter (fn eqn => getCon eqn = c) eqns

(* match : matchArg -> expression *)
(* If there are no equations, then the patterns did not match and the default
 * is returned.
 * If vars is nil, then all sources of discrimination have been exhausted
 * (implying null pats list in each equation), so only the 1st eqn is
 * relevant. If there is more than one equation, then the original match was
 * redundant. *)
fun match (nil, nil, default) = default       (* no live equations *)
  | match (nil, [(nil, exp)], default) = exp  (* single live equation *)
  | match (nil, eqns, default) = raise Fail "redundant equations?"
  | match (uvars, eqns, default) = 
     foldr (matchVarConTup uvars) default (partitionEqns eqns)

(* matchVarConTup : var list -> equation list * exp -> exp
 * REQUIRE: equation list not null
 * REQUIRE: eqns is a "block" of eqns of the same patKind
 * i.e. all equations in eqns have the same patKind, which
 * is therefore the patKind of first equation in the block. *)
and matchVarConTup (uvars: var list) (eqns: equation list, default: exp) =
    case eqnPatKind (hd eqns)
      of PKvar => matchVar (uvars, eqns, default)
       | PKcon => matchCon (uvars, eqns, default)
       | PKtup => matchTup (uvars, eqns, default)

(* matchVar : var list * equation list * exp -> exp *)
and matchVar (u::uvars, eqns, default) =
    let fun sub (PVAR v :: prest, exp) = (prest, subst(exp, u, v))
     in match (uvars, map sub eqns, default)
    end

(* matchTup : var list * equation list * exp -> exp *)
and matchTup (u::uvars, eqns, default) =
    let val PTUP pats = hd (#1 (hd eqns))  (* all eqns have same tuple arity *)
	val tuple_arity = length pats
	fun flatten ((PTUP new_pats)::pats, exp) = (new_pats @ pats, exp)
	    (* replace the initial tuple pat with its component pats *)
     in case pats  (* testing "arity" of tuple pattern *)
	 of nil =>  (* unit pattern -- drop it from each eqn
	             * in this case flatten simply drops the unit pattern *)
	      match (uvars, map flatten eqns, default)
	  | _ =>    (* n-tuple with n > 1 *)
	      let val new_uvars = List.tabulate (tuple_arity, (fn _ => newMVar()))
	       in DETUP (u, new_uvars, match (new_uvars @ uvars, map flatten eqns, default))
	      end
    end

(* matchCon : var list * equation list * exp -> exp *)
(*  all eqns are isCons (possibly with different constructors, possibly missing
 *  some constructors of the constructor family (i.e. datatype)).
 *  for the dcons list, we are using just those that actually appear in
 *  equations, but there may be more for a given datatype. So we are not
 *  dealing with generating default results for missing dcons (e.g. if
 *  CONS but not NIL appear). This is a bug! *)
and matchCon (u::uvars, eqns, default) =
    let val all_dcons = dconFamily (getCon (hd eqns))
	    (* should use full family of dcons for the type *)
        fun mkSRule dcon = 
	      matchSRule (dcon, uvars, choose (dcon, eqns), default)
     in SWITCH (u, map mkSRule all_dcons)
    end

(* matchSRule : dcon * vars * equations * exp -> srule
 *  all eqns are PKcon with the _same_ constructor
 *  matchSRule is called "matchClause" in Chapter 5 *)
and matchSRule (dcon, _, nil, default) = (dcon, NONE, default)  (* NONE ??? *)
  | matchSRule (dcon, uvars, eqns, default) =  (* eqns not null *)
    let val (new_uvars, newvarOp) =
            case fstPat eqns
	      of PCON(dcon, PTUP nil) => (uvars, NONE)
 	       | _ => let val v = newMVar () in (v::uvars, SOME v) end
        fun stripEqn (PCON (c, cpat)::pats, exp) =
	    case newvarOp
	      of NONE => (pats, exp)   (* constant constructor *)
               | _ => (cpat::pats, exp)
	val new_eqns = map stripEqn eqns
          (* cpat will be (PTUP nil) in the case where c is a constant constructor *)
     in (dcon, newvarOp, match (new_uvars, new_eqns, default))
    end

end (* top local *)
end (* structure MC *)
