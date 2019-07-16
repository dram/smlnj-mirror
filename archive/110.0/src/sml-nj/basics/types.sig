(* Copyright 1997 by Bell Laboratories *)
(* types.sig *)

signature TYPES =
sig

type label         
type polysign      

datatype eqprop = YES | NO | IND | OBJ | DATA | ABS | UNDEF

type tyvar                      

datatype litKind = INT | WORD | REAL | CHAR | STRING 

datatype openTvKind 
  = META 
  | FLEX of (label * ty) list   

and tvKind  
  = INSTANTIATED of ty
  | OPEN of {depth: int, eq: bool, kind: openTvKind}
  | UBOUND of {depth: int, eq: bool, name: Symbol.symbol}
  | LITERAL of {kind: litKind, region: SourceMap.region}
  | SCHEME of bool
  | LBOUND of {depth: DebIndex.depth, num: int}


and tycpath                        
  = TP_VAR of {depth: DebIndex.depth, num: int, kind: LtyKernel.tkind}
  | TP_TYC of tycon
  | TP_FCT of tycpath list * tycpath list
  | TP_APP of tycpath * tycpath list
  | TP_SEL of tycpath * int

and tyckind
  = PRIMITIVE of PrimTyc.primtyc
  | ABSTRACT of tycon
  | DATATYPE of
     {index: int,
      members: dtmember vector,
      lambdatyc: (LtyKernel.tyc * DebIndex.depth) option ref}
  | FLEXTYC of tycpath            
  | FORMAL                 
  | TEMP                   

and tycon
  = GENtyc of
      {stamp : Stamps.stamp, 
       arity : int, 
       eq    : eqprop ref,
       kind  : tyckind, 
       path  : InvPath.path}
  | DEFtyc of
      {stamp : Stamps.stamp, 
       tyfun : tyfun, 
       strict: bool list, 
       path  : InvPath.path}
  | PATHtyc of
      {arity : int,
       entPath : EntPath.entPath,
       path : InvPath.path}
  | RECORDtyc of label list
  | RECtyc of int
  | ERRORtyc

and ty 
  = VARty of tyvar
  | IBOUND of int                
  | CONty of tycon * ty list
  | POLYty of {sign: polysign, tyfun: tyfun}
  | WILDCARDty
  | UNDEFty

and tyfun 
  = TYFUN of {arity : int, body : ty}

(* datacon description used in dtmember *)
withtype dconDesc =
    {name: Symbol.symbol,
     rep: Access.conrep,
     domain: ty option}

(* member of a family of (potentially) mutually recursive datatypes *)
and dtmember =
    {tycname: Symbol.symbol,
     stamp: Stamps.stamp,
     arity: int,
     eq: eqprop ref,
     dcons: dconDesc list,
     sign: Access.consig,
     lambdatyc: (LtyKernel.tyc * DebIndex.depth) option ref}

val infinity : int
val mkTyvar  : tvKind -> tyvar
val copyTyvar : tyvar -> tyvar

datatype datacon                    (* data constructors *)
  = DATACON of
      {name   : Symbol.symbol,
       typ    : ty,
       rep    : Access.conrep,
       const  : bool,
       sign   : Access.consig}

end (* signature TYPES *)


(*
 * $Log: types.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.4  1997/09/05 04:35:44  dbm
 *   Added new function copyTyvar used in the fix for bug 1244.
 *
 * Revision 1.3  1997/03/22  17:57:56  dbm
 * Revision of type variables for better handling of literal overloading
 * and to fix bug 905/952.
 *
 * Revision 1.2  1997/03/17  18:46:41  dbm
 * Changes in datatype representation to support datatype replication.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:11  george
 *   Version 109.24
 *
 *)
