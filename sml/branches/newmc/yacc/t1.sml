(* t1.sml  -- based on yacc.grm.sml *)

structure Y =
struct

  type pos = int
  val lineno = ref 0
  val text = ref (nil: string list)

  val pr = fn out : TextIO.outstream => fn s : string => TextIO.output(out,s)

  datatype prec = LEFT | RIGHT | NONASSOC

  datatype symbol = SYMBOL of string * int

  type ty = string

  datatype control = NODEFAULT | VERBOSE | PARSER_NAME of symbol |
		     FUNCTOR of string  | START_SYM of symbol |
		     NSHIFT of symbol list | POS of string | PURE |
		     PARSE_ARG of string * string |
		     TOKEN_SIG_INFO of string

  datatype declData = DECL of
		  {eop : symbol list,
		   keyword : symbol list,
		   nonterm : (symbol*ty option) list option,
		   prec : (prec * (symbol list)) list,
		   change: (symbol list * symbol list) list,
		   term : (symbol* ty option) list option,
		   control : control list,
		   value : (symbol * string) list}

  type rhsData = {rhs:symbol list,code:string, prec:symbol option} list

  datatype rule = RULE of {lhs : symbol, rhs : symbol list,
			   code : string, prec : symbol option}

  datatype nonterm = NT of int

  datatype svalue
     = VOID
     | PROG of unit -> string
     | PREC of unit -> prec
     | INT of unit -> string
     | IDDOT of unit -> string
     | ID of unit -> string * int
     | HEADER of unit -> string
     | TYVAR of unit -> string
     | TY of unit -> string
     | CHANGE_DEC of unit ->  symbol list * symbol list
     | CHANGE_DECL of unit -> (symbol list * symbol list) list
     | SUBST_DEC of unit ->  symbol list * symbol list
     | SUBST_DECL of unit -> (symbol list * symbol list) list
     | G_RULE_PREC of unit -> symbol option
     | G_RULE_LIST of unit -> rule list
     | G_RULE of unit -> rule list
     | RHS_LIST of unit -> { rhs: symbol list, code:string, prec: symbol option } list
     | RECORD_LIST of unit -> string
     | QUAL_ID of unit -> string
     | MPC_DECLS of unit -> declData
     | MPC_DECL of unit -> declData
     | LABEL of unit -> string
     | ID_LIST of unit -> symbol list
     | CONSTR_LIST of unit -> (symbol * string option) list

  val defaultPos = 0

fun actions (state, stack) =
case (state,stack)

of  ( 0,      ( _, ( G_RULE_LIST g_rule_list1, _, right))
	   :: _
	   :: ( _, ( MPC_DECLS mpc_decls1, _, _))
	   :: ( _, ( HEADER header1, left, _))
	   :: rest) =>
     ( NT 0, ( VOID, left, right), rest)

 |  ( 1, ( ( _, ( MPC_DECL mpc_decl1, left1, right))
	:: ( _, ( MPC_DECLS mpc_decls1, left2, _))
	:: rest)) =>
     ( NT 5, ( VOID, left2, right), rest)

 |  ( 2, ( rest)) =>
    ( NT 5, ( VOID, defaultPos, defaultPos), rest)

 |  ( 3, ( ( _, ( CONSTR_LIST constr_list1, _, right))
	   :: ( _, ( _, left, _))
	   :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 4, ( ( _, ( CONSTR_LIST constr_list1, _, right))
	   :: ( _, ( _, left, _))
	   :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 5, ( ( _, ( ID_LIST id_list1, _, right))
	   :: ( _, ( PREC prec1, left, _))
	   :: rest)) =>
   ( NT 4, ( VOID, left, right), rest)

 |  ( 6, ( ( _, ( ID id1, _, right))
	   :: ( _, ( _, left, _))
	   :: rest)) => 
    ( NT 4, ( VOID, left, right), rest)

 |  ( 7, ( ( _, ( ID_LIST id_list1, _, right))
	   :: ( _, ( _, left, _))
	   :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 8, ( ( _, ( ID_LIST id_list1, _, right))
	   :: ( _, ( _, left, _))
	   :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 9, ( ( _, ( ID_LIST id_list1, _, right))
	   :: ( _, ( _, left, _))
	   :: rest)) => 
    ( NT 4, ( VOID, left, right), rest)

 |  ( 10, ( ( _, ( CHANGE_DECL change_decl1, _, right))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)
(*
 |  ( 11, ( ( _, ( SUBST_DECL subst_decl1, _, right))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 12, ( ( _, ( ID_LIST id_list1, _, right))
	    :: ( _ , ( _, left, _))
	    :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 13, ( ( _, ( PROG prog1, _, right))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 14, ( ( _, ( PROG prog1, _, right))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT 4, ( VOID, left, right) , rest)

 |  ( 15, ( ( _, ( ID id1, _, right))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 16, ( ( _, ( TY ty1, _, right))
	    :: _
	    :: ( _, (PROG prog1, _, _))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 17, ( ( _, ( _, left, right))
	    :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 18, ( ( _, ( _, left, right))
	    :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 19, ( ( _, ( _, left, right))
	    :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 20, ( ( _, ( TY ty1, _, right))
	    :: ( _, ( _, left, _))
	    :: rest)) => 
    ( NT 4, ( VOID, left, right), rest)

 |  ( 21, ( ( _, ( PROG prog1, _, right))
	    :: ( _, (ID ID1, _, _))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT 4, ( VOID, left, right), rest)

 |  ( 22, ( ( _, ( CHANGE_DECL change_decl1, _, right))
	    :: _
	    :: ( _, ( CHANGE_DEC change_dec1, left, _))
	    :: rest)) =>
    ( NT 14, ( VOID, left, right), rest)

 |  ( 23, ( ( _, ( CHANGE_DEC change_dec1, left, right))
	    :: rest)) =>
    ( NT 14, ( VOID, left, right), rest)

 |  ( 24, ( ( _, ( ID_LIST id_list2, _, right))
	    :: _
	    :: ( _, ( ID_LIST id_list1, left, _))
	    :: rest)) =>
    ( NT 15, ( VOID, left, right), rest)

 |  ( 25, ( ( _, ( SUBST_DECL subst_decl1, _, right))
	    :: _
	    :: ( _, ( SUBST_DEC subst_dec1, left, _))
	    :: rest)) =>
    ( NT 12, ( VOID, left, right), rest)

 |  ( 26, ( ( _, ( SUBST_DEC subst_dec1, left, right))
	    :: rest)) =>
    ( NT 12, ( VOID, left, right), rest)

 |  ( 27, ( ( _, ( ID id2, _, right))
	    :: _
	    :: ( _, (ID id1, left, _))
	    :: rest)) =>
    ( NT 13, ( VOID, left, right), rest)

 |  ( 28, ( ( _, ( TY ty1, _, right))
	    :: _
	    :: ( _, (ID id1, _, _))
	    :: _
	    :: ( _, ( CONSTR_LIST constr_list1, left, _))
	    :: rest)) =>
    ( NT 1, ( VOID, left, right), rest)

 |  ( 29, ( ( _, ( ID ID1, _, right))
	    :: _
	    :: ( _, (CONSTR_LIST constr_list1, left, _))
	    :: rest)) =>
    ( NT 1, ( VOID, left, right), rest)

 |  ( 30, ( ( _, ( TY ty1, _, right))
	    :: _
	    :: ( _, (ID id1, left, _))
	    :: rest)) =>
    ( NT 1, ( VOID, left, right), rest)

 |  ( 31, ( ( _, ( ID id1, left, right))
	    :: rest)) =>
    ( NT 1, ( VOID, left, right), rest)

 |  ( 32, ( ( _, ( RHS_LIST rhs_list1, _, right))
	    :: _
	    :: ( _, ( ID id1, left, _))
	    :: rest)) =>
    ( NT 9, ( VOID, left, right), rest)

 |  ( 33, ( ( _, ( G_RULE g_rule1, _, right))
	    :: ( _, (G_RULE_LIST g_rule_list1, left, _))
	    :: rest)) =>
    ( NT 10, ( VOID, left, right), rest)

 |  ( 34, ( ( _, ( G_RULE g_rule1, left, right))
	    :: rest)) =>
    ( NT 10, ( VOID, left, right), rest)

 |  ( 35, ( ( _, ( ID_LIST id_list1, _, right))
	    :: ( _ , ( ID id1, left, _))
	    :: rest)) =>
    ( NT 2, ( VOID, left, right), rest)

 |  ( 36, rest) =>
    ( NT 2, ( VOID, defaultPos, defaultPos), rest)

 |  ( 37, ( ( _, ( PROG prog1, _, right))
	    :: ( _, (G_RULE_PREC g_rule_prec1, _, _))
	    :: ( _, ( ID_LIST id_list1, left, _))
	    :: rest)) =>
    ( NT 8, ( VOID, left, right), rest)

 |  ( 38, ( ( _, ( PROG prog1, _, right))
	    :: ( _, (G_RULE_PREC g_rule_prec1, _, _))
	    :: ( _, ( ID_LIST id_list1, _, _))
	    :: _
	    :: ( _, ( RHS_LIST rhs_list1, left, _))
	    :: rest)) =>
    ( NT 8, ( VOID, left, right), rest)

 |  ( 39, ( ( _, ( TYVAR tyvar1, left, right))
	    :: rest)) =>
    ( NT 16, ( VOID, left, right), rest)

 |  ( 40, ( ( _, ( _, _, right))
	    :: ( _, ( RECORD_LIST record_list1, _, _))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT 16, ( VOID, left, right), rest)

 |  ( 41, ( ( _, ( _, _, right)) 
	   :: ( _, ( _, left, _))
	   :: rest)) =>
    ( NT 16, ( VOID, left, right), rest)

 |  ( 42, ( ( _, ( PROG prog1, left, right))
	    :: rest)) =>
    ( NT 16, ( VOID, left, right), rest)

 |  ( 43, ( ( _, ( QUAL_ID qual_id1, _, right))
	    :: ( _ , ( TY TY1, left, _))
	    :: rest)) =>
    ( NT 16, ( VOID, left, right), rest)

 |  ( 44, ( ( _, ( QUAL_ID qual_id1, left, right))
	    :: rest)) =>
    ( NT 16, ( VOID, left, right), rest)

 |  ( 45, ( ( _, ( TY ty2, _, right))
	    :: _
	    :: ( _, (TY ty1, left, _))
	    :: rest)) => 
    ( NT 16, ( VOID, left, right), rest)

 |  ( 46, ( ( _, ( TY ty2, _, right))
	    :: _
	    :: ( _, (TY ty1, left, _))
	    :: rest)) =>
    ( NT 16, ( VOID, left, right), rest)

 |  ( 47, ( ( _, ( TY ty1, _, right))
	    :: _
	    :: ( _, (LABEL label1, _, _))
	    :: _
	    :: ( _, ( RECORD_LIST record_list1, left, _))
	    :: rest)) =>
    ( NT 7, ( VOID, left, right), rest)

 |  ( 48, ( ( _, ( TY ty1, _, right))
	    :: _
	    :: ( _, (LABEL label1, left, _))
	    :: rest)) =>
    ( NT 7, ( VOID, left, right), rest)

 |  ( 49, ( ( _, ( ID id1, left, right)) :: rest)) =>
    ( NT 6, ( VOID, left, right), rest)

 |  ( 50, ( ( _, ( QUAL_ID qual_id1, _, right))
	    :: ( _ , ( IDDOT iddot1, left, _))
	    :: rest)) =>
    ( NT 6, ( VOID, left, right), rest)

 |  ( 51, ( ( _, ( ID id1, left, right)) :: rest)) =>
    ( NT 3, ( VOID, left, right), rest)

 |  ( 52, (( _, ( INT int1, left, right)) :: rest)) =>
    ( NT 3, ( VOID, left, right), rest)

 |  ( 53, (( _, ( ID id1, _, right)) :: ( _, ( _, left, _)) :: rest)) =>
    ( NT 11, ( VOID, left, right), rest)

 |  ( 54, rest) =>
    ( NT 11, ( VOID, defaultPos, defaultPos), rest)
*)

 | _ => raise (Fail "rule 55")

end (* structure Y *)
