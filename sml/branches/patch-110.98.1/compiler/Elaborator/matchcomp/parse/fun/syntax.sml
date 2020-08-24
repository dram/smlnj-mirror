(* syntax.sml *)

(* abstract syntax for Fun (typed lambda calculus with recursion, products, sums,
 * recursive types, and polymorphism) *)

structure Syntax =
struct

(* variables: used for both expression variables and type variables *)
type variable = string

(* abs: a type constructor for the common pattern of a variable paired
 * with its scope expression. This is the general form where the abstracted
 * variable is not given a type. *)
type 'a abs = variable * 'a

(* In order to define the ilist recursive type, we add UNITty as a
 * type constant, and Unit as an expression constant. *)

(* Abstract Syntax: type expressions *)
datatype ty
  = UNITty | INTty | BOOLty (* primitive types, Unit, Int, Bool *)
  | FUNty of ty * ty        (* function types ty1 -> ty2 *)
  | PRODty of ty * ty       (* product types  ty1 * ty2  *)
  | SUMty of ty * ty        (* sum types ty1 + ty2 *)
  | VARty of variable       (* type variable in recursive types *)
  | RECty of ty abs         (* mu types, i.e. recursive types *)
  | POLYty of ty abs        (* polymorphic types *)
  | ERRORty

(* expr: Abstract Syntax of Expressions
 * (1) There are three separate constant forms: Unit, Num, and Bool.
 * (2) Primops are predefined variables bound in an initial environment.
 * (3) The type of a let-bound variable can be computed from the definiens expr,
 *     so let expressions do not require a type ascription for the bound variable.
 * (4) Letrec expressions are represented by ordinary let expressions where
 *     the definiens is a Rec (mu) expression denoting a recursive fixed point.
 *     The Rec bound variable must be typed, hence we use a tabs.
 * (5) The body expression of a Rec must be an explicit function abstraction (Fun).
 *)

(* typed variable abstractions *)
type 'a tabs = variable * ty * 'a

datatype expr
  = Unit
     (* ()  <==>  Unit *)
  | Num of int           (* number constants - integers *)
     (* n  <==>  Num(|n|) *)
  | Bool of bool         (* boolean constants *)
     (* b  <==>  Bool(|b|) *)
  | Var of variable      (* these will include predefined primop variables *)
     (* x  <==>  Var("x") *)
  | If of expr * expr * expr
     (* if e1 then e2 else e3  <==>  If(e1,e2,e3) *)
 (* local variable declarations *)
  | Let of expr * expr abs
     (* let x = e1 in e2  <==>  Let(e1,(x,e2)) *)
  | Letrec of (expr tabs * expr) tabs
     (* letrec f: ty1 = fn y:ty2 => e1 in e2  <==>  Letrec(f,ty1,((y,ty2,e1), e2))
        where ty1 should be FUNty(ty2,ty3) *)
 (* function abstraction and application *)
  | Fun of expr tabs
     (* fn x: ty => e  <==>  Fun (x,ty,e) *)
  | App of expr * expr
     (* e1 e2  <==>  App(e1,e2) *)
 (* products *)
  | Pair of expr * expr
     (* (e1,e2)  <==>  Pair(e1,e2) *)
 (* sums *)
  | Case of expr * expr abs * expr abs
     (* case e1 of Inl x => e2 | Inr y => e3  <==>  Case(e1,(x,e2),(y,e3)) *)
 (* recursive type isomorphisms *)
  | Fold of ty * expr
     (* Fold[ty] e  <==>  Fold(ty,e) *)
  | Unfold of expr
     (* Unfold e  <==>  Unfold e *)
 (* polymorphic type abstraction *)
  | TFun of expr abs
     (* Fn t => e  <==>  TFun (t,e) *)
  | TApp of expr * ty
     (* e[ty]  <==>  TApp(e,ty) *)

datatype decl
  = ValDef of variable * expr
  | FunDef of variable * variable * ty * ty * expr

datatype stmt
  = Decl of decl
  | Expr of expr
  | Quit

end (* structure Syntax *)
