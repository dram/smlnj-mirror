(* mc/lib/types.sml *)

structure Types =
struct

type stamp = Stamp.stamp

datatype typevar
  = TYvar of
    {name : string,
     id : int}

datatype tycon
  = Tycon of
    {name : string,
     stamp : stamp,
     arity : int,
     kind : tycKind}

and tycKind
    = PRIM
    | DATA of datacon list ref

and metaKind
  = INST of ty
  | META
	
and ty
  = UNDEFty
  | TYVAR of typevar         (* externally bound "real" type variable *)
  | METAVAR of metavar
  | DBI of int               (* de Bruijn index for polymorphic bound variable *)
  | CONty of tycon * ty list

and polyTy
  = POLY of
      {arity : int,
       body : ty}

and datacon
  = DCON of
    {name: string,
     stamp : stamp,
     owner : tycon,  (* always a datatype tycon *)
     polyty : polyTy}

withtype metavar = metaKind ref
			
type label = string

(* mkLabel : string -> label *)
fun mkLabel (s: string) : label = s

end (* structure Types *)
