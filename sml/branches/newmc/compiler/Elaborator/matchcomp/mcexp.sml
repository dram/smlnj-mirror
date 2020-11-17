(* mcexp.sml *)

(* mcexp: abstract syntax of expressions to represent the action of a match. *)

structure MCexp =
struct

local
  structure T = Types
  structure SV = SVar
  structure V = VarCon
in

(* "bodies" of expressions for rule RHSs will be Absyn.exp,
 * representing a "raw" RHS of a rule. *)

datatype mcexp
  = Var of SV.svar  (* how/where used? *)
      (* "sv" as an expression *)
  | Letr of SV.svar list * SV.svar * mcexp
      (* "let (sv1, ..., svn) = sv0 in body"; destructure a record *)
  | Letv of SV.svar list * SV.svar * mcexp
      (* "let (sv1, ..., svn) = sv0 in body"; destructure a vector *)
  | Letf of SV.svar * mcexp * mcexp
      (* "let f = << fn sv => fbody >> in body" *)
      (* 1st mcexp will always be an Sfun. The function will be a
       * functionalized RHS. *)
  | Letm of V.var list * SV.svar list * Absyn.exp
      (* "let (v1, ..., vn) = (sv1, ..., svn) in rhsexp" *)
      (* non-functionalized, single use, rule RHS, with linkage for svars *)
  | Case of SV.svar * (MCTypes.key * SV.svar option * mcexp) list * mcexp option
      (* Destructure an OR, with svar binding if key is not a constant. As a special case
       * it is also used for SINGLE datacons. *)
  | Sfun of V.var list * Absyn.exp
      (* "fn (v0, ..., vn) => body"; functionalized, multi-use rule RHS *)
  | Sapp of SV.svar * SV.svar list
      (* "sv0 (sv1, ..., svn)";  A-normal-style: function and args have all been
       * bound to svars. Will only be used for instances of functionaolized RHSs,
       * so sv0 will be bound (using Letf) to a RHS function. *)
  | Tfun of T.tyvar list * mcexp
      (* "TFN (tyv0, ..., tyvn) => body"; type function with tyvar parameters. *)
      (* ??? type-level abstraction may be left to Translate phase? *)
  | Failure
      (* "raise MATCH"; May be redundant if matches guaranteed exhaustive. *)

end (* local *)
end (* structure MCexp *)

(* NOTE: we don't need letm case if we translate svars to corresponding vars while
 * translating the body of the letm (using map from svar to (var,rule)), and keeping
 * track of which rule rhs we are translating. *)
(* NOTE: all Case forms will be exhaustive, with a default created for cases where the
 * explicit keys are not exhaustive (in particular for int, word, string constants,
 * and cases where not all datacons are explicitly present.). Code generation/translation
 * will represent these defaults as additional rules with a wildcard pattern. *)
(* NOTE: could have added a boolean flag or andKind flag to letr and thus merged
 * Letr and Letv. *)

(* NOTE: The mcexp datatype is "emulated" in VMCexp (vmcexp.sml) using functions
 * with the same names as the mcexp constructors that produce Absyn.exp expressions
 * instead of mcexp expressions. *)
 
