(* typesutil.sml *)

structure TypesUtil =
struct

local open Types in

  fun bug msg = ErrorMsg.impossible msg
				  
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

  fun datatypeWidth (Tycon{kind=DATA dcons,...}) =
      length (!dcons)
    | datatypeWidth _ = bug "datatypeWidth applied to non-datatype"

  (* dataconWidth : datacon -> int *)
  fun dataconWidth (DCON{owner,...}: datacon) = datatypeWidth owner

  fun dataconType (DCON{polyty,...}) = polyty
				       
  fun mkPrimTycon (name, arity) =
      Tycon{name = name,
	    stamp = Stamp.new(),
	    arity = arity,
	    kind = PRIM}

  fun mkDataTycon (name, arity) =
      let val dcons = ref nil
      in (Tycon {name = name,
		 stamp = Stamp.new(),
		 arity = arity,
		 kind = DATA dcons},
	  dcons)
      end

  (* instantiatePoly : polyTy * ty list -> ty *)
  fun instantiatePoly (POLY{arity,body}, argtys: ty list) =
      let fun subst(DBindex n) = List.nth(argtys,n)
	    | subst(CONty(tyc,args)) = CONty(tyc, map subst args)
	    | subst ty = ty
       in if arity <> length argtys
	    then bug "applyTyfun: arity mismatch"
	  else if arity > 0
	    then subst body
	  else body
      end

  (* mkTupleTycon : int -> tycon *)
  fun mkTupleTycon (n: int) : tycon =
      mkPrimTycon(Int.toString n ^ "-tuple", n)

  val tupleTycons = Vector.tabulate (20, mkTupleTycon)

  fun tupleTycon n = Vector.sub(tupleTycons, n)

  (* T.ty * int -> T.ty list *)
  fun replicateTy (ty,len) =
      let fun build (0,tys) = tys
            | build (n,tys) = build (n-1, ty::tys)
      in build (len,nil)
      end
	  
  fun mkTupleTy (tys: ty list) =
      CONty(tupleTycon (length tys), tys)

  val funTycon = mkPrimTycon("->", 2)
  fun funTy (ty1, ty2) = CONty(funTycon, [ty1,ty2])

  fun domain_range (CONty(tycon,[domain,range])) =
      if equalTycon(tycon, funTycon)
      then (domain,range)
      else bug "domain_range: arg not a function type"
    | domain_range _ = bug "domain_range: unexpected type"
			   
  (* matchPoly : T.ty * T.PolyTy -> T.ty vector *)
  fun matchPoly (ty, POLY{arity,body}) =
      let val instArray = Array.array(arity,UNDEFty)
          fun match (ty, DBindex i) =
	      if equalType(ty, Array.sub(instArray, i))
	      then ()
	      else Array.update(instArray, i, ty)
	    | match (CONty(tyc1,args1), (CONty(tyc2,args2))) =
	      if equalTycon (tyc1,tyc2)
	      then ListPair.appEq match (args1, args2)
	      else bug "matchPoly 1"
	    | match _ = bug "matchPoly 2"
      in match(ty, body);
	 Array.vector(instArray)
      end

  (* instTy : ty vector -> ty -> ty *)
  fun instTy vec (CONty(tyc, args)) = CONty(tyc, map (instTy vec) args)
    | instTy vec (DBindex i) = Vector.sub(vec,i)
    | instTy vec ty = ty

  (* destructCon : T.ty * dcon -> T.ty
   * given an instance of the range of the type of a dcon, returns corresponding instance
   * of the domain of the dcon *)
  fun destructCon(ty,dcon) =
      (* ASSERT: dcon is not a constant dcon, and ty is the range of an instance of 
       * the (possibly polymorphic) type of dcon *)
      let val POLY{arity,body} = dataconType dcon
	  val (domain, range) = domain_range body (* dconTy assumed to be a function type *)
	  val instVector = matchPoly (ty, POLY{arity=arity, body=range})
      in instTy instVector domain
      end
	  
  fun destructRecord (CONty(_, elemTys)) = elemTys

  val tyvarCount = ref 0 (* a global variable *)
  fun newTypeVar () =
      let val id = !tyvarCount before (tyvarCount := !tyvarCount + 1)
	  val prefix = str(Char.chr(Char.ord #"a" + id mod 26))
	  val suffix = if (id >= 26) then Int.toString(id mod 26) else ""
	  val name = concat["'", prefix, suffix]
      in TYvar{name = name, id = id}
      end

  (* instantiatePoly : polyTy -> typevar list * ty *)
  fun instantiatePoly (POLY{arity,body}) =
      let val newTypeVars = List.tabulate(arity, (fn i => newTypeVar()))
	  fun inst (CONty(tyc, args)) = CONty(tyc, map inst args)
	    | inst (DBindex i) = TYVAR(List.nth(newTypeVars, i))
	    | inst ty = ty
      in (newTypeVars, inst body)
      end

end (* local *)
end (* structure TypesUtil *)
