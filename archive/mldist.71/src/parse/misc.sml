(* Copyright 1989 by AT&T Bell Laboratories *)
(* misc.sml *)

structure Misc : MISC =
struct

  open Array List ErrorMsg Symbol PrintUtil Access Basics BasicTypes
       TypesUtil Absyn EnvAccess 
  infix 9 sub

  val ASTERISKsym = Symbol.varSymbol "*"
  val EQUALsym = Symbol.varSymbol "="
 
  fun for l f = app f l

  local fun uniq ((a0 as (a,_,_))::(r as (b,_,_)::_)) = 
		    if Symbol.eq(a,b) then uniq r else a0::uniq r
	  | uniq l = l
      fun gtr((a,_,_),(b,_,_)) = 
		     let val a' = Symbol.name a and b' = Symbol.name b
		         val zero = ord "0" and nine = ord "9"
			 val a0 = ordof(a',0) and b0 = ordof(b',0)
		      in if a0 >= zero andalso a0 <= nine
			  then if b0 >= zero andalso b0 <= nine
				 then size a' > size b' orelse
					  size a' = size b' andalso a' > b'
				 else false
			  else if b0 >= zero andalso b0 <= nine
				then true
				else a' > b'
		     end
   in val sort3 = uniq o Sort.sort gtr
  end

  (* following could go in Absyn *)
  val bogusID = Symbol.varSymbol "bogus"
  val bogusExnID = Symbol.varSymbol "Bogus"
  val bogusExp = VARexp(ref(VALvar{name=[bogusID],typ=ref ERRORty,
			           access=PATH[0]}))

  val anonParamName = Symbol.strSymbol "AnonParam"

  val nullSigStamp = Stampset.newStamp(Stampset.sigStamps)
  val nullSigStampsets = Stampset.newStampsets()
  val nullStrenv = REL{s=arrayoflist [NULLstr,NULLstr], t=arrayoflist []}
  val nullSig = 
      STRstr{stamp = Stampset.newStamp(#strStamps nullSigStampsets),
             sign = nullSigStamp,
	     table = Env.empty,
	     env = nullStrenv,
	     kind = SIGkind{share = {s=[],t=[]},
		            bindings = [],
			    stamps = nullSigStampsets}}
  val nullStr = 
      STRstr{stamp = Stampset.newStamp(Stampset.fixedStrStamps),
             sign = nullSigStamp,
	     table = Env.empty,
	     env = nullStrenv,
	     kind = STRkind{path=[Symbol.strSymbol "NullStructure"]}}
  val nullParamVar = STRvar{name=[anonParamName],
			    access=PATH[namedLvar(anonParamName)],
			    binding=nullSig}

  fun discard _ = ()

  fun single x = [x]

  fun varcon (VARbind v) = VARexp(ref v)
    | varcon (CONbind d) = CONexp d
    | varcon _ = impossible "Misc.varcon"

  fun checkbound(used,bound,err) =
    let open TyvarSet
	val boundset = fold (fn (v,s) => union_tyvars(singleton_tyvar v,s)) 
							  bound no_tyvars
	fun nasty(ref(INSTANTIATED(VARty v))) = nasty v
	  | nasty(ref(UBOUND{name,...})) = 
	     err COMPLAIN ("unbound type variable " ^ Symbol.name name ^
			  " in type declaration")
	  | nasty _ = err BUG "corelang.checkbound"
     in if length bound > length(get_tyvars boundset)
	  then err COMPLAIN "duplicate bound type variable in type or datatype"
	  else ();
	app nasty (get_tyvars(diff_tyvars(used, boundset)))
    end

end (* structure Misc *)
