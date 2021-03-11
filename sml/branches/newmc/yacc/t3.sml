(* t3.sml  -- based on yacc.grm.sml *)

structure Y =
struct

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

  type spp = svalue * int * int
  type stack = spp list

fun actions (state: int, stack: stack): spp * stack =
case (state,stack)

of  (0,       (G_RULE_LIST g_rule_list1, _, right)
	   :: _
	   :: (MPC_DECLS mpc_decls1, _, _)
	   :: (HEADER header1, left, _)
	   :: rest) =>
     ((VOID, left, right), rest)

 | (1,     (MPC_DECL mpc_decl1, left1, right)
	:: (MPC_DECLS mpc_decls1, left2, _)
	:: rest) =>
     ((VOID, left2, right), rest)

 | (2, rest) =>
    ((VOID, defaultPos, defaultPos), rest)

 | (3, (CONSTR_LIST constr_list1, _, right)
	   :: (_, left, _)
	   :: rest) =>
    ((VOID, left, right), rest)

 | (4, (CONSTR_LIST constr_list1, _, right)
	   :: (_, left, _)
	   :: rest) =>
    ((VOID, left, right), rest)

 | (5, (ID_LIST id_list1, _, right)
	   :: (PREC prec1, left, _)
	   :: rest) =>
   ((VOID, left, right), rest)

 | (6, (ID id1, _, right)
	   :: (_, left, _)
	   :: rest) => 
    ((VOID, left, right), rest)

 | (7, (ID_LIST id_list1, _, right)
    :: (_, left, _)
    :: rest) =>
    ((VOID, left, right), rest)

 | (8, (ID_LIST id_list1, _, right)
    :: (_, left, _)
    :: rest) =>
    ((VOID, left, right), rest)

 | (9, (ID_LIST id_list1, _, right)
    :: (_, left, _)
    :: rest) => 
    ((VOID, left, right), rest)

 | (10, (CHANGE_DECL change_decl1, _, right)
     :: (_, left, _)
     :: rest) =>
    ((VOID, left, right), rest)

 | (11, (SUBST_DECL subst_decl1, _, right)
     :: (_, left, _)
     :: rest) =>
    ((VOID, left, right), rest)

 | (12, (ID_LIST id_list1, _, right)
     :: ( _ ,left, _)
     :: rest) =>
    ((VOID, left, right), rest)

 | (13, (PROG prog1, _, right)
     :: (_, left, _)
     :: rest) =>
    ((VOID, left, right), rest)

 |  ( 14, (PROG prog1, _, right)
	    :: (_, left, _)
	    :: rest) =>
    ((VOID, left, right) , rest)

 |  ( 15, (ID id1, _, right)
	    :: (_, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)
(*
 |  ( 16, (TY ty1, _, right)
	    :: _
	    :: ((PROG prog1, _, _)
	    :: (_, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 17, (_, left, right)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 18, (_, left, right)
       :: rest) =>
    ((VOID, left, right), rest)

 |  ( 19, (_, left, right)
       :: rest) =>
    ((VOID, left, right), rest)

 |  ( 20, (TY ty1, _, right)
	    :: (_, left, _)
	    :: rest) => 
    ((VOID, left, right), rest)

 |  ( 21, (PROG prog1, _, right)
	    :: ((ID ID1, _, _)
	    :: (_, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 22, (CHANGE_DECL change_decl1, _, right)
	    :: _
	    :: (CHANGE_DEC change_dec1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 23, (CHANGE_DEC change_dec1, left, right)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 24, (ID_LIST id_list2, _, right)
	    :: _
	    :: (ID_LIST id_list1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 25, (SUBST_DECL subst_decl1, _, right)
	    :: _
	    :: (SUBST_DEC subst_dec1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 26, (SUBST_DEC subst_dec1, left, right)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 27, (ID id2, _, right)
	    :: _
	    :: ((ID id1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 28, (TY ty1, _, right)
	    :: _
	    :: ((ID id1, _, _)
	    :: _
	    :: (CONSTR_LIST constr_list1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 29, (ID ID1, _, right)
	    :: _
	    :: ((CONSTR_LIST constr_list1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 30, (TY ty1, _, right)
	    :: _
	    :: ((ID id1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 31, (ID id1, left, right)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 32, (RHS_LIST rhs_list1, _, right)
	    :: _
	    :: (ID id1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 33, (G_RULE g_rule1, _, right)
	    :: ((G_RULE_LIST g_rule_list1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 34, (G_RULE g_rule1, left, right)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 35, (ID_LIST id_list1, _, right)
	    :: (ID id1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 36, rest) =>
    ((VOID, defaultPos, defaultPos), rest)

 |  ( 37, ( (PROG prog1, _, right)
	    :: ((G_RULE_PREC g_rule_prec1, _, _)
	    :: (ID_LIST id_list1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 38, ( (PROG prog1, _, right)
	    :: ((G_RULE_PREC g_rule_prec1, _, _)
	    :: (ID_LIST id_list1, _, _)
	    :: _
	    :: (RHS_LIST rhs_list1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 39, ( (TYVAR tyvar1, left, right)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 40, ( ((_, right)
	    :: (RECORD_LIST record_list1, _, _)
	    :: (_, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 41, ( ((_, right) 
	   :: (_, left, _)
	   :: rest) =>
    ((VOID, left, right), rest)

 |  ( 42, ( (PROG prog1, left, right)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 43, ( (QUAL_ID qual_id1, _, right)
	    :: ( _ , ( TY TY1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 44, ( (QUAL_ID qual_id1, left, right)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 45, ( (TY ty2, _, right)
	    :: _
	    :: ((TY ty1, left, _)
	    :: rest) => 
    ((VOID, left, right), rest)

 |  ( 46, ( (TY ty2, _, right)
	    :: _
	    :: ((TY ty1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 47, ( (TY ty1, _, right)
	    :: _
	    :: ((LABEL label1, _, _)
	    :: _
	    :: (RECORD_LIST record_list1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 48, ( (TY ty1, _, right)
	    :: _
	    :: ((LABEL label1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 49, ( (ID id1, left, right) :: rest) =>
    ((VOID, left, right), rest)

 |  ( 50, ( (QUAL_ID qual_id1, _, right)
	    :: ( _ , ( IDDOT iddot1, left, _)
	    :: rest) =>
    ((VOID, left, right), rest)

 |  ( 51, ( (ID id1, left, right) :: rest) =>
    ((VOID, left, right), rest)

 |  ( 52, ( (INT int1, left, right) :: rest) =>
    ((VOID, left, right), rest)

 |  ( 53, ( (ID id1, _, right) :: (_, left, _) :: rest) =>
    ((VOID, left, right), rest)

 |  ( 54, rest) =>
    ((VOID, defaultPos, defaultPos), rest)
*)

 | _ => raise (Fail "rule 55")

end (* structure Y *)
