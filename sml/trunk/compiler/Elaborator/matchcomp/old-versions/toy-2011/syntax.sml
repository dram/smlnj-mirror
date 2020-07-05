(* syntax.sml *)

structure Syntax =
struct

(* values for integer literals *)
type literal = int

(* identifiers *)
(* there are four classes of identifiers, and for the time
 * being we choose to represent them all as symbols *)
type var = Symbol.symbol   (* value variables *)
type tyv = Symbol.symbol   (* type variables *)
type tyc = Symbol.symbol   (* type constructor names *)
type con = Symbol.symbol  (* data constructor names *)


(* TYPES *)
datatype ty 
    = VarTy of tyv	        (* type variable *)
    | ConTy of tyc * ty list	(* type constructor application *)
    | TupleTy of ty list	(* tuple *)

datatype typDec
  = TypeDec of                  (* type dec *)
     {tyc : tyc,
      params: tyv list,
      defn: ty}
  | DataDec of			(* datatype dec *)
     {tyc: tyc, 
      params: tyv list,
      datacons: consdec list}

(* DATACON DECLS *)
withtype consdec = {con : con, typ: ty option}


(* EXPRESSIONS *)
datatype exp
  = VarExp of var		(* variable *)
  | IntExp of literal		(* integer *)
  | StrExp of string		(* string *)
  | FnExp of bindpat * exp	(* function abstraction *)
  | AppExp of			(* application *)
     {func: exp,
      arg: exp}
  | CaseExp of		        (* case expression *)
     {expr: exp, 
      match: rule list}
  | LetExp of                   (* let expression *)
     {dec: valDec list,
      body: exp}
  | SeqExp of exp list		(* sequence of expressions *)
  | TupleExp of exp list	(* tuple (derived form) *)
  | IfExp of			(* if expression (derived form) *)
     {cond: exp,
      thenCase: exp,
      elseCase: exp}

(* PATTERNS *)
and atompat
  = WildPat				(* empty pattern *)
  | VarPat of var			(* variable pattern *)
  | IntPat of literal			(* integer *)
  | StrPat of string			(* string *)
  | ConPat of con                       (* constructor constant, eg. Nil *)

and bindpat
  = AtomPat of atompat
  | TuplePat of atompat list

and rulepat 
  = BindPat of bindpat
  | AppPat of
     {constr:con, argument: bindpat} 	(* constructor application *)

(* VALUE DECLARATIONS *)
and valDec
  = ValDec of (bindpat * exp)      (* values *)
  | FunDec of (funbind list)   	(* recursive functions *)

(* RECURSIVE FUNCTION BINDINGS *)
withtype funbind = {fname : Symbol.symbol, param: bindpat, body: exp}

(* case rules *)
and rule = {pat:rulepat, exp:exp}

datatype topDec
  = Ty of typDec
  | Val of valDec

type program = topDec list * exp
 
end (* structure Ast *)

