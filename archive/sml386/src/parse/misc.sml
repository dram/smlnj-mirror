(* Copyright 1989 by AT&T Bell Laboratories *)
(* misc.sml *)

structure Misc : MISC =
struct

  open ErrorMsg Symbol PrtUtil Access Basics BasicTyp
       TypesUtl Absyn EnvAcc EnvAcc.Env

  val ASTERISKsym = Symbol.symbol "*"
  val EQUALsym = Symbol.symbol "="
 
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

  fun protect((enter,exit),doit) =
      let val t = enter()
       in (doit() before exit t)
           handle exn => (exit t; raise exn)
      end

  val protectScope = (openScope,resetEnv)

  (* following could go in Absyn *)
  val bogusID = Symbol.symbol "bogus"
  val bogusExnID = Symbol.symbol "Bogus"
  val bogusExp = VARexp(ref(VALvar{name=[bogusID],typ= ref ERRORty,
			           access=LVAR(mkLvar())}))

  val anonName = Symbol.symbol "Anon"
  val anonParamName = Symbol.symbol "AnonParam"

  val nullSigStamp = Stampset.newStamp(Stampset.sigStamps)
  val nullSigStampsets = Stampset.newStampsets()
  val nullStrenv = REL{s=arrayoflist [NULLstr,NULLstr], t=arrayoflist []}
  val nullSig = 
      STRstr{stamp = Stampset.newStamp(#strStamps nullSigStampsets),
             sign = nullSigStamp,
	     table = newTable(),
	     env = nullStrenv,
	     kind = SIGkind{share = {s=[],t=[]},
		            bindings = [],
			    stamps = nullSigStampsets}}
  val nullStr = 
      STRstr{stamp = Stampset.newStamp(Stampset.fixedStrStamps),
             sign = nullSigStamp,
	     table = newTable(),
	     env = nullStrenv,
	     kind = STRkind{path=[Symbol.symbol "NullStructure"]}}
  val nullParamVar = STRvar{name=[anonParamName],
			    access=LVAR(namedLvar(anonParamName)),
			    binding=nullSig}

  fun discard _ = ()

  fun single x = [x]

  fun varcon (VARbind v) = VARexp(ref v)
    | varcon (CONbind d) = CONexp d
    | varcon _ = impossible "parse.39"

fun getSTRpath([id],err) = (lookSTR id
		 handle Unbound => 
		    (err COMPLAIN ("unbound structure name: " ^ name id);
		     bogusSTR))
  | getSTRpath(q,err) = lookPathSTR(q,err COMPLAIN)


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
