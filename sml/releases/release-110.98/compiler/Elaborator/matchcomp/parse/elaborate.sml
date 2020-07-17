(* elaborate.sml *)

(* elaborate pattern syntax trees (syntax.sml) into Absyn.pat in the
 * context of an environment mapping identifiers to constructors *)

structure ElaboratePat =
struct

local
  structure T = Types
  structure V = Var
  structure LV = LambdaVar
  structure A = Absyn
  structure S = PatSyntax
in

(* association list environments -- could use Atom and AtomMap *)
type env = string * T.datacon

(* insert : env * string * T.datacon -> env *)
fun insert(e,s,d) = (s,d)::e

(* lookup : env * string -> T.datacon option *)
fun lookup ((s1,d1)::e, s) =
    if s = s1 then SOME d1
    else lookup(e,s)
  | lookup (nil,s) = NONE

fun elaborate (S.Id s, env) = 
    (case lookup(env,s)
      of NONE =>
	 if s = "_" then A.WILDpat
	 else A.VARpat(V.mkVALvar(Symbol.make s, LV.mkLvar ()))
      | SOME dcon => A.CONpat(dcon,[]))
  | elaborate (S.NumPat s, env) = 
      (case IntInf.fromString s
         of NONE => raise Fail "bad int string in NumPat"
          | SOME n => A.NUMpat(s,{ival=n,ty=BasicTypes.intTy}))
  | elaborate (S.AppPat(d,pat), env) =
      (case lookup(env,d)
	of NONE => raise Fail "undefined datacon name"
	 | SOME dcon => A.APPpat(dcon,[],elaborate(pat,env)))
  | elaborate (S.OrPat(pat1,pat2),env) = 
      A.ORpat(elaborate(pat1,env), elaborate(pat2,env))
  | elaborate (S.TuplePat pats, env) =
      Setup.mkTuplePat(map (fn p => elaborate(p,env)) pats)

end (* local *)
end (* structure ElaboratePat *)
