(* Copyright 1996 by AT&T Bell Laboratories *)
(* elabtype.sig *)

signature ELABTYPE =
sig

  val elabType :
        Ast.ty * StaticEnv.staticEnv * ErrorMsg.errorFn * SourceMap.region
        -> Types.ty * TyvarSet.tyvarset

  val elabTyvList : 
        Ast.tyvar list * ErrorMsg.errorFn * SourceMap.region 
        -> Types.tyvar list

  val elabDB :
        (Types.tyvar list * Symbol.symbol 
                  * (Symbol.symbol * Ast.ty option) list * SourceMap.region) 
        * StaticEnv.staticEnv * InvPath.path * ErrorMsg.errorFn
        -> VarCon.datacon list * StaticEnv.staticEnv

  val elabTYPEdec :
        Ast.tb list * StaticEnv.staticEnv * InvPath.path 
        * SourceMap.region * ElabUtil.compInfo
        -> Absyn.dec * StaticEnv.staticEnv

  val elabDATATYPEdec :
        {datatycs: Ast.db list, withtycs: Ast.tb list} * StaticEnv.staticEnv 
        * ExpandTycon.sigContext * EntityEnv.entityEnv * InvPath.path
	* SourceMap.region * ElabUtil.compInfo
        -> Types.tycon list * Types.tycon list * VarCon.datacon list 
           * StaticEnv.staticEnv

  val debugging : bool ref

end (* signature ELABTYPE *)

(*
 * $Log: elabtype.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:45  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/02/26 15:35:40  dbm
 * Fix bug 1141.  Added entityEnv parameter to elabDATATYPEdec.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:35  george
 *   Version 109.24
 *
 *)
