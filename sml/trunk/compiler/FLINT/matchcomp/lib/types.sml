(* mc/lib/types.sml *)

structure Types =
struct

type stamp = Stamp.stamp

type tyvar = {stamp : stamp}

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
  | TYVAR of tyvar
  | METAVAR of metatyvar
  | DBVAR of int
  | CONty of tycon * ty list

and polyTy
  = POLY of
      {arity : int,
       body : ty}

and datacon
  = DCON of
    {name: Symbol.symbol,
     stamp : stamp,
     owner : tycon,
     type : polyTy}

withtype metatyvar = tyvarKind ref
			
type label = string

(* mkLabel : string -> label *)
fun mkLabel (s: string) : label = s

end (* structure Types *)


structure TypesUtil =
struct

local open Types in

  (* equalTycon : tycon * tycon -> bool *)
  fun equalTycon (Tycon{stamp=s1,...}, Tycon{stamp=s2,...}) =
      Stamp.same(s1,s2)

  (* equalType : ty * ty -> bool *)
  fun equalType (ty1: Types.ty, ty2: Types.ty) =
      case (ty1,ty2)
       of (UNDEFty, UNDEFty) => true
	| (CONty(tyc1,args1), CONty(tyc2,args2)) =>
	  equalTycon(tyc1, tyc2) andalso ListPair.allEq equalType (args1,args2)

  (* dataconEq : datacon * datacon -> bool *)
  fun dataconEq (DCON{stamp = s1, ...}: datacon, DCON{stamp = s2,...}: datacon) =
      Stamp.same(s1,s2)

  (* dataconName : datacon -> Symbol.symbol *)
  fun dataconName (DCON{name,...}: datacon) = name

  fun datatypeWidth (Tycon{kind=DATA(dcons),...}) =
      length (!dcons)
    | dataconWidth _ = raise Fail "dataconWidth"

  (* dataconWidth : datacon -> int *)
  fun dataconWidth (DCON{owner,...}: datacon) = datatypeWidth owner

  fun mkPrimTycon (name, arity) =
      Tycon{name = name,
	    stamp = Stamp.new(),
	    arity = arity,
	    tycKind = PRIM}

  fun mkDataTycon (name, arity) =
      let val dcons = ref nil
      in (Tycon {name = name,
		 stamp = stamp,
		 arity = arity,
		 kind = DATA dcons},
	  dcons)
      end

  (* instantiatePoly : polyTy * ty list -> ty *)
  fun instantiatePoly (POLYty{arity,body}, argtys: ty list) =
      let fun subst(DBVAR n) = List.nth(args,n)
	    | subst(CONty(tyc,args)) = CONty(tyc, map subst args)
	    | subst ty = ty
       in if arity <> length args
	    then bug "applyTyfun: arity mismatch"
	  else if arity > 0
	    then subst body
	  else body
      end

  (* mkTupleTycon0 : int -> tycon *)
  fun mkTupleTycon0 (n: int) : tycon =
      mkPrimTycon(Int.toString n ^ "-tuple", n)

  val tupleTycons = Vector.tabulate (20, mkTupleTycon0)
(*
      #[mkTupleTycon0 2, mkTupleTycon0 3, mkTupleTycon0 4, mkTupleTycon0 5, 
	mkTupleTycon0 6, mkTupleTycon0 7, mkTupleTycon0 8, mkTupleTycon0 9, 
	mkTupleTycon0 10, mkTupleTycon0 11, mkTupleTycon0 12, mkTupleTycon0 13] 
*)
  fun tupleTycon n = Vector.sub(tupleTycons, n)

  (* T.ty * int -> T.ty list *)
  fun replicateTy (ty,len) =
      let fun build (0,tys) = tys
            | build (n,tys) = build (n-1, ty:tys)
      in build (n,nil)
      end
	  
  fun mkTupleTy (tys: ty list) =
      CONty(tupleTycon (length tys), tys)

end (* local *)
end (* structure TypesUtil *)

structure BasicTypes =
struct

local open Types in

val intTycon = mkPrimTycon ("int",0)
val int32Tycon = mkPrimTycon "int32"
val int64Tycon = mkPrimTycon "int64"
val intinfTycon = mkPrimTycon "intinf"
val wordTycon = mkPrimTycon "word"
val word8Tycon = mkPrimTycon "word8"
val word32Tycon = mkPrimTycon "word32"
val word64Tycon = mkPrimTycon "word64"

val intTy = CONty(intTycon, [])
val int32Ty = CONty(int32Tycon, [])
val int64Ty = CONty(int64Tycon, [])
val intinfTy = CONty(intinfTycon, [])
val wordTy = CONty(wordTycon, [])
val word8Ty = CONty(word8Tycon, [])
val word32Ty = CONty(wordTycon, [])
val word64Ty = CONty(wordTycon, [])
		  
val funTycon = mkPrimTycon("->", 2)

val (boolTycon, boolDcons) = mkDataTycon("bool", 0)
val boolTy = POLYty{arity = 0, body = CONty(boolTycon, nil)}
val trueDcon = DCon{name = "true",
		    stamp = Stamp.new(),
		    owner = boolTycon,
		    type = boolTy}
val falseDcon = DCon{name = "false",
		     stamp = Stamp.new(),
		     owner = boolTycon,
		     type = boolTy}
val _ = boolDcons := [trueDcon, falseDcon]

val (listTycon, listDcons) = mkDataTycon("list", 1)
val nilTy = POLYty{arity = 1, body = CONty(listTycon, DBVAR 0)}
val consTy = POLYty{arity = 1,
		    body = CONty(funTycon,
				 [CONty(tupleTycon 2, [DBVAR 0,
						      CONty(listTycon, DBVAR 0)]),
				  CONty(listTycon, DBVAR 0)])}
val nilDcon = DCon{name = "Nil",
		   stamp = Stamp.new(),
		   owner = listTycon,
		   type = nilTy}
val consDcon = DCon{name = "Cons",
		    stamp = Stamp.new(),
		    owner = listTycon,
		     type = consTy}
val _ = listDcons := [nilDcon, consDcon]
			

end (* local *)
end (* structure BasicTypes *)
