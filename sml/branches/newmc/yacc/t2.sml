(* t2.sml  -- based on yacc.grm.sml *)

structure Y =
struct

  datatype nonterm = NT

  datatype svalue
     = VOID
     | PROG of unit -> unit
     | PREC of unit -> unit
     | INT of unit -> unit
     | IDDOT of unit -> unit
     | ID of unit -> unit
     | HEADER of unit -> unit
     | TYVAR of unit -> unit
     | TY of unit -> unit
     | CHANGE_DEC of unit -> unit
     | CHANGE_DECL of unit -> unit
     | SUBST_DEC of unit -> unit
     | SUBST_DECL of unit -> unit
     | G_RULE_PREC of unit -> unit
     | G_RULE_LIST of unit -> unit
     | G_RULE of unit -> unit
     | RHS_LIST of unit -> unit
     | RECORD_LIST of unit -> unit
     | QUAL_ID of unit -> unit
     | MPC_DECLS of unit -> unit
     | MPC_DECL of unit -> unit
     | LABEL of unit -> unit
     | ID_LIST of unit -> unit
     | CONSTR_LIST of unit -> unit

  val defaultPos = 0

  type stack = (nonterm * (svalue * int * int)) list

fun actions (state: int, stack: stack): stack =
case (state,stack)

of  ( 0,      ( _, ( G_RULE_LIST g_rule_list1, _, right))
	   :: _
	   :: ( _, ( MPC_DECLS mpc_decls1, _, _))
	   :: ( _, ( HEADER header1, left, _))
	   :: rest) =>
     ( NT, ( VOID, left, right), rest)

 |  ( 1, ( ( _, ( MPC_DECL mpc_decl1, left1, right))
	:: ( _, ( MPC_DECLS mpc_decls1, left2, _))
	:: rest)) =>
     ( NT, ( VOID, left2, right), rest)

 |  ( 2, ( rest)) =>
    ( NT, ( VOID, defaultPos, defaultPos), rest)

 |  ( 3, ( ( _, ( CONSTR_LIST constr_list1, _, right))
	   :: ( _, ( _, left, _))
	   :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 4, ( ( _, ( CONSTR_LIST constr_list1, _, right))
	   :: ( _, ( _, left, _))
	   :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 5, ( ( _, ( ID_LIST id_list1, _, right))
	   :: ( _, ( PREC prec1, left, _))
	   :: rest)) =>
   ( NT, ( VOID, left, right), rest)

 |  ( 6, ( ( _, ( ID id1, _, right))
	   :: ( _, ( _, left, _))
	   :: rest)) => 
    ( NT, ( VOID, left, right), rest)

 |  ( 7, ( ( _, ( ID_LIST id_list1, _, right))
	   :: ( _, ( _, left, _))
	   :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 8, ( ( _, ( ID_LIST id_list1, _, right))
	   :: ( _, ( _, left, _))
	   :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 9, ( ( _, ( ID_LIST id_list1, _, right))
	   :: ( _, ( _, left, _))
	   :: rest)) => 
    ( NT, ( VOID, left, right), rest)

 |  ( 10, ( ( _, ( CHANGE_DECL change_decl1, _, right))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 11, ( ( _, ( SUBST_DECL subst_decl1, _, right))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 12, ( ( _, ( ID_LIST id_list1, _, right))
	    :: ( _ , ( _, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 13, ( ( _, ( PROG prog1, _, right))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 14, ( ( _, ( PROG prog1, _, right))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right) , rest)

 |  ( 15, ( ( _, ( ID id1, _, right))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)
(*
 |  ( 16, ( ( _, ( TY ty1, _, right))
	    :: _
	    :: ( _, (PROG prog1, _, _))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 17, ( ( _, ( _, left, right))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 18, ( ( _, ( _, left, right))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 19, ( ( _, ( _, left, right))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 20, ( ( _, ( TY ty1, _, right))
	    :: ( _, ( _, left, _))
	    :: rest)) => 
    ( NT, ( VOID, left, right), rest)

 |  ( 21, ( ( _, ( PROG prog1, _, right))
	    :: ( _, (ID ID1, _, _))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 22, ( ( _, ( CHANGE_DECL change_decl1, _, right))
	    :: _
	    :: ( _, ( CHANGE_DEC change_dec1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 23, ( ( _, ( CHANGE_DEC change_dec1, left, right))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 24, ( ( _, ( ID_LIST id_list2, _, right))
	    :: _
	    :: ( _, ( ID_LIST id_list1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 25, ( ( _, ( SUBST_DECL subst_decl1, _, right))
	    :: _
	    :: ( _, ( SUBST_DEC subst_dec1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 26, ( ( _, ( SUBST_DEC subst_dec1, left, right))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 27, ( ( _, ( ID id2, _, right))
	    :: _
	    :: ( _, (ID id1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 28, ( ( _, ( TY ty1, _, right))
	    :: _
	    :: ( _, (ID id1, _, _))
	    :: _
	    :: ( _, ( CONSTR_LIST constr_list1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 29, ( ( _, ( ID ID1, _, right))
	    :: _
	    :: ( _, (CONSTR_LIST constr_list1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 30, ( ( _, ( TY ty1, _, right))
	    :: _
	    :: ( _, (ID id1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 31, ( ( _, ( ID id1, left, right))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 32, ( ( _, ( RHS_LIST rhs_list1, _, right))
	    :: _
	    :: ( _, ( ID id1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 33, ( ( _, ( G_RULE g_rule1, _, right))
	    :: ( _, (G_RULE_LIST g_rule_list1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 34, ( ( _, ( G_RULE g_rule1, left, right))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 35, ( ( _, ( ID_LIST id_list1, _, right))
	    :: ( _ , ( ID id1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 36, rest) =>
    ( NT, ( VOID, defaultPos, defaultPos), rest)

 |  ( 37, ( ( _, ( PROG prog1, _, right))
	    :: ( _, (G_RULE_PREC g_rule_prec1, _, _))
	    :: ( _, ( ID_LIST id_list1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 38, ( ( _, ( PROG prog1, _, right))
	    :: ( _, (G_RULE_PREC g_rule_prec1, _, _))
	    :: ( _, ( ID_LIST id_list1, _, _))
	    :: _
	    :: ( _, ( RHS_LIST rhs_list1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 39, ( ( _, ( TYVAR tyvar1, left, right))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 40, ( ( _, ( _, _, right))
	    :: ( _, ( RECORD_LIST record_list1, _, _))
	    :: ( _, ( _, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 41, ( ( _, ( _, _, right)) 
	   :: ( _, ( _, left, _))
	   :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 42, ( ( _, ( PROG prog1, left, right))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 43, ( ( _, ( QUAL_ID qual_id1, _, right))
	    :: ( _ , ( TY TY1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 44, ( ( _, ( QUAL_ID qual_id1, left, right))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 45, ( ( _, ( TY ty2, _, right))
	    :: _
	    :: ( _, (TY ty1, left, _))
	    :: rest)) => 
    ( NT, ( VOID, left, right), rest)

 |  ( 46, ( ( _, ( TY ty2, _, right))
	    :: _
	    :: ( _, (TY ty1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 47, ( ( _, ( TY ty1, _, right))
	    :: _
	    :: ( _, (LABEL label1, _, _))
	    :: _
	    :: ( _, ( RECORD_LIST record_list1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 48, ( ( _, ( TY ty1, _, right))
	    :: _
	    :: ( _, (LABEL label1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 49, ( ( _, ( ID id1, left, right)) :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 50, ( ( _, ( QUAL_ID qual_id1, _, right))
	    :: ( _ , ( IDDOT iddot1, left, _))
	    :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 51, ( ( _, ( ID id1, left, right)) :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 52, (( _, ( INT int1, left, right)) :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 53, (( _, ( ID id1, _, right)) :: ( _, ( _, left, _)) :: rest)) =>
    ( NT, ( VOID, left, right), rest)

 |  ( 54, rest) =>
    ( NT, ( VOID, defaultPos, defaultPos), rest)
*)

 | _ => raise (Fail "rule 55")

end (* structure Y *)
