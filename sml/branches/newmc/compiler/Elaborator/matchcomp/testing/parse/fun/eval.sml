(* eval.sml *)

(* Big step, CBV, environment-based evaluator for the Fun language *)

signature EVAL =
sig
  datatype value
    (* primitive values *)
    = UNIT
    | NUM of int
    | BOOL of bool
    (* function values *)
    | FUN of Syntax.variable * Syntax.expr * env
    (* recursive function closures -- only used in env bindings *)
    | FUNrec of Syntax.variable * Syntax.expr * env
    | PRIM of value -> value           (* primitive operators *)
    (* products: pairs *)
    | PAIR of value * value
    (* disjoint sums: Inl and Inr constructors *)
    | INL of value
    | INR of value
    (* recursive type values *)
    | FOLD of value
    (* polymorphic abstractions *)
    | TFUN of Syntax.expr * env
    (* error value *)
    | ERROR
  withtype env = value Env.env

  val venvInit : env

  val eval0 : Syntax.expr * env -> value

  exception Exit

  datatype stmtValue
    = ExprVal of value
    | DeclVal of Syntax.variable * value

  val evalStmt: Syntax.stmt * env -> stmtValue * env

end (* signature EVAL *)

structure Eval : EVAL =
struct

structure E = Env

open Syntax

datatype value
  (* primitive values *)
  = UNIT
  | NUM of int
  | BOOL of bool
  (* function values *)
  | FUN of variable * expr * env
  (* recursive function closures -- only used in env bindings *)
  | FUNrec of variable * expr * env  (* only used in env bindings *)
  | PRIM of value -> value           (* primitive operators *)
  (* compound values: pairs *)
  | PAIR of value * value
  (* disjoint sums: Inl and Inr constructors *)
  | INL of value
  | INR of value
  (* recursive type values *)
  | FOLD of value
  (* polymorphic abstractions *)
  | TFUN of expr * env
  (* error value *)
  | ERROR

withtype env = value E.env

exception Error of string
fun error (msg: string) = raise Error msg

(* primitive operations *)
fun primError (prim: string) =
    error ("primop args(" ^ prim ^ ")")

(* the following are the functions (of type value -> value), implementing
 * the primitive operations, with dynamic type checking of the arguments *)
fun plus(PAIR(NUM n1, NUM n2))  = NUM(n1 + n2)
  | plus _ = primError "plus" 
fun times(PAIR(NUM n1, NUM n2)) = NUM(n1 * n2)
  | times _ = primError "times" 
fun minus(PAIR(NUM n1, NUM n2)) = NUM(n1 - n2)
  | minus _ = primError "minus" 
fun equal(PAIR(NUM n1, NUM n2)) = BOOL(n1 = n2)
  | equal _ = primError "equal"
fun lt(PAIR(NUM n1, NUM n2))    = BOOL(n1 < n2)
  | lt _ = primError "lt"
fun gt(PAIR(NUM n1, NUM n2))    = BOOL(n1 > n2)
  | gt _ = primError "gt"
fun fst(PAIR(v1,v2)) = v1
  | fst _ = primError "Fst"
fun snd(PAIR(v1,v2)) = v2
  | snd _ = primError "Snd"
fun inl(v) = INL v
fun inr(v) = INR v

val primNames = ["+","*","-","==","<",">","fst","snd","Inl","Inr"]
val primValues = [plus,times,minus,equal,lt,gt,fst,snd,inl,inr]

(* initial environment *)
val venvInit = ListPair.foldr E.bind E.empty (primNames, map PRIM primValues)

(* eval0: expr * env -> value;
 *   Assume: expr argument is a closed expression
 *)
fun eval0(Unit, _) = UNIT
  | eval0(Num n, _) = NUM n
  | eval0(Bool b, _) = BOOL b
  | eval0(Var x, env) =
      (case E.look(x,env)
         of v as FUNrec(y,e,env') => FUN(y,e,E.bind(x,v,env'))
          | v => v)
  | eval0(If(e1,e2,e3), env) = 
      (case eval0 (e1,env)
         of BOOL true => eval0 (e2,env)
	  | BOOL false => eval0 (e3,env)
          | _ => error "If - cond not boolean")
  | eval0(Letrec(f,_,((y,_,defbody), body)), env) =
      eval0(body, E.bind(f,FUNrec(y,defbody,env), env))
  | eval0(Let(def,(x,body)), env) =
      eval0(body, E.bind(x,eval0(def,env),env))
  | eval0(Fun(x,ty,e), env) = FUN(x,e,env)
  | eval0(App(e1,e2), env) =
     (case eval0 (e1,env)
        of FUN(x,body,env') => eval0(body, E.bind(x, eval0(e2,env), env'))
         | PRIM primop => primop(eval0(e2, env))
         | _ => error "App - operator not a function")

  (* products *)
  | eval0(Pair(e1,e2), env) =
     PAIR(eval0(e1,env), eval0(e2,env))

  (* disjoint sums *)
  | eval0(Case(e1,(x,e2),(y,e3)),env) = 
     (case eval0(e1,env)
        of INL v => eval0(e2,Env.bind(x,v,env))
         | INR v => eval0(e3,Env.bind(y,v,env))
         | _ => error "Case - scrutinee not of sum type")

  (* recursive types *)
  | eval0(Fold (_,e),env) = FOLD (eval0 (e,env))
  | eval0(Unfold e,env) = 
     (case eval0(e,env)
        of FOLD v => v
         | _ => error "Unfold applied to a non-fold value")

  (* polymorphic abstraction and application *)
  | eval0(TFun(t,e),env) = TFUN(e,env)
  | eval0(TApp(e,ty),env) =
     (case eval0(e,env)
        of TFUN(e1,env1) => eval0(e1,env1)
         | v as PRIM _ => v
         | _ => error "TApp - ill-formed operator")

fun eval1 (expr,venv) =
    eval0(expr,venv)
    handle Error msg =>
      (print ("Error: "^msg^"\n"); ERROR)

(* evaluating statements *)

exception Exit

(* "values" of statements *)
datatype stmtValue
  = ExprVal of value
  | DeclVal of variable * value

(* evalDecl : decl * value Env.env -> stmtValue * value Env.env *)
fun evalDecl (ValDef(v,e),env) = 
    (case eval1(e, env)
       of ERROR => (DeclVal(v,ERROR), env)
        | def => (DeclVal(v,def), E.bind(v, def, env)))
  | evalDecl (FunDef(f,x,ty_arg,ty_res,e),env) = 
    let val fval = FUNrec(x,e,env)
     in (DeclVal(f,fval), E.bind(f, fval, env))
    end

(* evalStmt : stmt * value Env.env -> (stmtValue * value Env.env) *)
fun evalStmt (Decl d, env) = evalDecl(d, env)
  | evalStmt (Expr e, env) = (ExprVal(eval1(e, env)), env)
  | evalStmt (Quit,_) = raise Exit
    
end (* structure Eval *)
