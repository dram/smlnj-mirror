
(* Copyright 1989 by AT&T Bell Laboratories *)
(* typesutil.sml *)

structure TypesUtil : TYPESUTIL = struct

structure Types = Types

open Array List PrintUtil Types List2 ErrorMsg BasicTypes
infix 9 sub (* stupid! *)

(* primitive operations on tycons *)

fun impossibleTyc s =
    fn (GENtyc _) => impossible (s ^ " GENtyc")
     | (DEFtyc _) => impossible (s ^ " DEFtyc")
     | (RECORDtyc _) => impossible (s ^ " RECORDtyc")
     | (RELtyc _) => impossible (s ^ " RELtyc [something]")
     | (ABSFBtyc _) => impossible (s ^ " ABSFBtyc [something]")
     | (FORMtyc l) => impossible (s ^ " FORMtyc [something]")
     | (ERRORtyc) => raise ErrorMsg.Cascade (s ^ " ERRORtyc")
     | _ => raise ErrorMsg.Cascade (s ^ " [some tyc]")

fun tycName(tycon) =	(* fix this some day to return entire path *)
    case tycon
      of GENtyc{path=name::_,...} => name
       | DEFtyc{path=name::_,...} => name
       | _ => impossibleTyc "tycName" tycon

fun tyconArity(GENtyc{arity,...}) = arity
  | tyconArity(RECORDtyc l) = length l
  | tyconArity(DEFtyc{tyfun=TYFUN{arity,...},...}) = arity
  | tyconArity(FORMtyc{spec,...}) = tyconArity spec
  | tyconArity(OPENFORMtyc{spec,...}) = tyconArity spec
  | tyconArity(ERRORtyc) = 0
  | tyconArity tycon = impossibleTyc "tyconArity" tycon

fun tycPath (GENtyc{path,...}) = path
  | tycPath (DEFtyc{path,...}) = path
  | tycPath tycon  = impossibleTyc "tycPath" tycon

fun setTycPath(tycon,path) =
    case tycon
      of GENtyc{stamp,arity,eq,kind,...} =>
	   GENtyc{stamp=stamp,path=path,arity=arity,eq=eq,kind=kind}
       | DEFtyc{tyfun,strict,...} => DEFtyc{tyfun=tyfun,path=path,strict=strict}
       | _ => impossibleTyc "setTycName" tycon

fun eqTycon(GENtyc{stamp=s,...},GENtyc{stamp=s',...}) = s=s'
  | eqTycon(RECORDtyc l1, RECORDtyc l2) = l1=l2
  | eqTycon(ERRORtyc,_) = true
  | eqTycon(_,ERRORtyc) = true
  | eqTycon(t1 as DEFtyc{strict=strict1,...},
	    t2 as DEFtyc{strict=strict2,...}) =
      strict1 andalso strict2 andalso t1 = t2
  | eqTycon(t1,t2) = (t1 = t2) 
    (* this case used in PrintBasics to check data constructors of
       a datatype.  Anywhere else??? *)

(* relativize type to structure context, represented by the strenv *)
(* could optimize to share; possible to merge with applyPoly ??? *)
fun rewriteType getTycon =
    let fun typeInContext ty =
	let fun mapTycons ty =
		case ty
		 of VARty(ref(INSTANTIATED ty')) => mapTycons ty'
		  | FLEXRECORDty(ref(CLOSED ty')) => mapTycons ty'
		  | VARty _ => ty
		  | CONty(tyc, argtys) =>
		      CONty(getTycon tyc, map mapTycons argtys)
		  | POLYty{sign,tyfun=TYFUN{arity,body},abs} =>
		      POLYty{sign=sign, abs=abs,
			     tyfun=TYFUN{arity=arity, body=mapTycons body}}
		  | _ => impossible "typeInContext"
	 in mapTycons ty
	end
     in typeInContext
    end

fun prune(VARty(tv as ref(INSTANTIATED ty))) : ty =
      let val pruned = prune ty
       in tv := INSTANTIATED pruned; pruned
      end
  | prune(FLEXRECORDty(r as ref(CLOSED(ty)))) =
      let val pruned = prune ty
       in r := CLOSED pruned; pruned
      end
  | prune ty = ty
    
fun eqTyvar(tv1: tyvar, tv2: tyvar) = (tv1 = tv2)

fun mkUBOUND(id : Symbol.symbol,err: ErrorMsg.complainer) : tvkind =
    let val s = Symbol.name id
	val (start,eq) = if substring(s,1,1) = "'" handle Substring => false
				then (2,true) else (1,false)
	val s = if ordof(s,start)=Ascii.underscore handle Ord => false
		  then if Ascii.isDigit(ordof(s,start+1)) handle Ord => false
			then substring(s,0,start)^"1"^
				 substring(s,start,size s - (start+1))
			else substring(s,0,start)^"1"^
				 substring(s,start+1,size s - (start+1))
		  else s
        fun digits(i,n) = 
	    let val c = ordof(s,i)
	     in if Ascii.isDigit(c)
		then digits(i+1,10*n+c-Ascii.zero)
		else n
	    end
	    handle Ord => n
	fun weak_num start = 
	   let val n = digits(start,0) handle Overflow => infinity
	      in if n > infinity div 2 
		     then err COMPLAIN "weakness number too large in type variable"
		   else ();
		 n
	     end
        val weakness = if Ascii.isDigit(ordof(s,start)) handle Ord => false
		         then weak_num start
		         else infinity
     in UBOUND{name=Symbol.tyvSymbol s,depth=infinity,weakness=weakness,eq=eq}
    end

fun bindTyvars(tyvars: tyvar list) : unit =
    let fun loop([],_) = ()
	  | loop(tv::rest,n) =
	      (tv := IBOUND n;
	       loop(rest,n+1))
     in loop(tyvars,0)
    end

fun bindTyvars1(tyvars: tyvar list) : {weakness:int,eq:bool} list =
    let fun loop([],_) = []
	  | loop((tv as ref(UBOUND{weakness,eq,...}))::rest,n) =
	       (tv := IBOUND n;
	        {weakness=weakness,eq=eq} :: loop(rest,n+1))
     in loop(tyvars,0)
    end

exception SHARE

(* assume that f fails on identity, i.e. f x raises SHARE instead of 
   returning x *)
fun shareMap f nil = raise SHARE
  | shareMap f (x::l) =
      (f x) :: ((shareMap f l) handle SHARE => l)
      handle SHARE => x :: (shareMap f l)

fun applyTyfun(TYFUN{arity,body},args) =
    let fun subst(VARty(ref(IBOUND n))) = nth(args,n)
	  | subst(CONty(tyc,args)) = CONty(tyc, shareMap subst args)
	  | subst(VARty(ref(INSTANTIATED ty))) = subst ty
	  | subst(FLEXRECORDty(ref(CLOSED ty))) = subst ty
	  | subst _ = raise SHARE
     in if arity > 0
	then subst body
	     handle SHARE => body
		  | Nth => ERRORty (* not enough arguments -- arity mismatch *)
	else body
    end

exception ReduceType

fun reduceType(CONty(DEFtyc{tyfun,...}, args)) = applyTyfun(tyfun,args)
  | reduceType(VARty(ref(INSTANTIATED ty))) = ty
  | reduceType _ = raise ReduceType

fun headReduceType ty = headReduceType(reduceType ty) handle ReduceType => ty

fun equalType(ty,ty') =
    let fun eq(VARty(tv),VARty(tv')) =
	      eqTyvar(tv,tv') orelse
	      (case (!tv,!tv')
	         of (IBOUND i1, IBOUND i2) => i1 = i2
		  | _ => false)
	  | eq(ty as CONty(tycon, args), ty' as CONty(tycon', args')) =
	      if eqTycon(tycon, tycon') then List2.all2 equalType(args,args') 
	      else (eq(reduceType ty, ty')
		    handle ReduceType =>
		      (eq(ty,reduceType ty') handle ReduceType => false))
	  | eq(VARty _, CONty _) =
	      (eq(ty,reduceType ty')
	       handle ReduceType => false)
	  | eq(CONty _, VARty _) =
	      (eq(reduceType ty, ty')
	       handle ReduceType => false)
	  | eq(ERRORty,_) = true
	  | eq(_,ERRORty) = true
	  | eq _ = false
     in eq(prune ty, prune ty')
    end

local
    fun makeDummyType() =
	CONty(GENtyc{stamp = Stamps.newFree(),
		     path = [Symbol.tycSymbol "dummy"], arity = 0,
		     eq = ref YES, kind = ref(PRIMtyc)},
	      [])
    fun makeargs 0 = []
      | makeargs i = makeDummyType() :: makeargs(i-1)
    val args = makeargs 10
    fun dargs(0,_,d) = d
      | dargs(n,a::r,d) = dargs(n-1,r,a::d)
      | dargs(n,[],d) = dargs(n-1,[],makeDummyType()::d)
 in fun dummyargs n = dargs(n,args,[])
end

fun equalTycon(ERRORtyc,_) = true
  | equalTycon(_,ERRORtyc) = true
  | equalTycon(t1 as RELtyc _,t2) = t1=t2
  | equalTycon(t1,t2 as RELtyc _) = t1=t2
  | equalTycon(t1,t2) =
     let val a1 = tyconArity t1 and a2 = tyconArity t2
     in if a1<>a2 then false
        else
	  let val args = dummyargs a1
	  in equalType(CONty(t1,args),CONty(t2,args))
	  end
     end

(* instantiating polytypes *)

val defaultMETA = META{depth=infinity,weakness=infinity,eq=false}

fun typeArgs n = 
    if n>0
    then VARty(mkTyvar defaultMETA) :: typeArgs(n-1)
    else []

val default_tvprop = {weakness=infinity,eq=false}

fun mkPolySign 0 = []
  | mkPolySign n = default_tvprop :: mkPolySign(n-1)

(* matching a scheme against a target type -- used declaring overloadings *)
fun matchScheme(TYFUN{arity,body}: tyfun, target: ty) : ty =
    let val tyenv = array(arity,UNDEFty)
	fun listofarray a =
	    let fun loop i = (a sub i)::loop(i+1) handle Subscript => []
	     in loop 0
	    end
	fun matchTyvar(i:int, ty: ty) : unit = 
	    case tyenv sub i
	      of UNDEFty => update(tyenv,i,ty)
	       | ty' => if equalType(ty,ty')
			then () 
			else impossible("matchScheme: bad tyvar "^makestring i)
        fun match(scheme:ty, target:ty) =
	    case (scheme,prune(target))
	      of (VARty(ref(IBOUND i)),ty) => matchTyvar(i,ty)
	       | (CONty(tycon1,args1), pt as CONty(tycon2,args2)) =>
		   if eqTycon(tycon1,tycon2)
		   then app2 match (args1, args2)
		   else (match(reduceType scheme, target)
			 handle ReduceType =>
			   (match(scheme, reduceType pt)
			    handle ReduceType =>
			      impossible "matchScheme: match -- tycons "))
	       | _ => impossible "matchScheme: match"
     in case prune target
	  of POLYty{sign,tyfun=TYFUN{arity=arity',body=body'},abs} =>
	       (match(body,body');
	        POLYty{sign = sign, abs=abs,
		       tyfun = TYFUN{arity = arity',
			             body = if arity>1
					    then tupleTy(listofarray tyenv)
					    else tyenv sub 0}})
	   | ty => 
	       (match(body,ty);
	        if arity>1
		then tupleTy(listofarray tyenv)
		else tyenv sub 0)
    end

val rec compressTy =
   fn t as VARty(x as ref(INSTANTIATED(VARty(ref v)))) => (x := v; compressTy t)
    | CONty(tyc,tyl) => app compressTy tyl
    | FLEXRECORDty(ref(CLOSED ty)) => compressTy ty
    | FLEXRECORDty(ref(OPEN(l,_))) => app (compressTy o #2) l
    | POLYty{tyfun=TYFUN{body,...},...} => compressTy body
    | _ => ()


(* dbm, 6/23/90: constructor TOP added to properly identify toplevel and
   fix bug 224. *)
abstype absp
	= RATOR of occ (* {lamd: int, absd: int, base: int, wmax: int, outer: absp} *)
	| ABSTR | LET | TOP
      and occ = OCC of {lamd: int, absd: int, base: int, wmax: int, outer: absp}
with
 fun Rand(OCC{outer=RATOR x,...}) = Rand(x)
   | Rand(OCC{lamd,absd,base,wmax,outer}) =
       OCC{lamd=lamd,absd=absd,base=base,wmax=min(wmax,absd),outer=outer}
 fun Abstr(OCC{lamd,wmax,outer=RATOR(OCC{absd,base,outer,...}),...}) =
       OCC{lamd=lamd+1,absd=absd,base=base,wmax=wmax,outer=outer}
   | Abstr(OCC{lamd,absd,base,wmax,...}) =
       OCC{lamd=lamd+1,absd=absd+1,base=base+1,wmax=wmax,outer=ABSTR}
 fun Rator(x as OCC{lamd,absd,wmax,base,...}) =
       OCC{lamd=lamd,absd=absd-1,base=base,wmax=wmax,outer=RATOR x}
 fun LetDef(OCC{lamd,absd,wmax,base,...}) =
       OCC{lamd=lamd,absd=absd,base=base,wmax=infinity,outer=LET}
 val Root = OCC{lamd=0,absd=0,base=0,wmax=infinity,outer=TOP}
 fun lamdepth (OCC{lamd,...}) = lamd
 fun abscount (OCC{absd,...}) = absd
 fun base     (OCC{base,...}) = base
 fun wmax     (OCC{wmax,...}) = wmax
 fun toplevel (OCC{outer=TOP,...}) = true | toplevel _ = false

 (* can't be merged with typeInContext and used in varApplied, etc. *)
 fun applyPoly(POLYty{sign,tyfun,abs}, OCC{absd,base,wmax,...}) : ty =
     let val args =
	     map (fn {weakness,eq} => 
		   VARty(ref(META{weakness =
				    if weakness >= infinity
				    then infinity
				    else min(max(weakness+absd-abs,base),wmax),
				  depth = infinity,
				  eq = eq})))
		 sign
     in  applyTyfun(tyfun, args)
     end
   | applyPoly(ty,_) = ty

 (* instantiateType: ty * occ -> ty
    New function to instantiate type of applied occurrences of variables.
    Called in Typecheck.expType.
    This adjusts weakness of free META type variables in addition to 
    calculating the weakness of instantiations of the bound type variables.
 *)
 fun instantiateType(POLYty{sign,tyfun=TYFUN{body,...},abs}: ty,
		     OCC{absd,base,wmax,...}) : ty =
     (* all variable types assumed to be polytypes.  This is so the
	abs value will be stored in the type for use here. *)
     let val args =
	     map (fn {weakness,eq} => 
		   VARty(ref(META{weakness =
				    if weakness >= infinity
				    then infinity
				    else min(max(weakness+absd-abs,base),wmax),
				  depth = infinity,
				  eq = eq})))
		  sign
	 fun subst(VARty(ref(IBOUND n))) = nth(args,n)
	   | subst(ty as VARty(r as ref(META{weakness,depth,eq}))) =
	       let val newWeakness =
			if weakness >= infinity
			then infinity
			else min(max(weakness+absd-abs,base),wmax)
	       in  if newWeakness < weakness
		   then (r := META{weakness=newWeakness, depth=depth, eq=eq}; ty)
		   else raise SHARE
	       end
	   | subst(CONty(tyc,args)) = CONty(tyc, shareMap subst args)
	   | subst(VARty(ref(INSTANTIATED ty))) = subst ty
	   | subst(FLEXRECORDty(ref(CLOSED ty))) = subst ty
	   | subst _ = raise SHARE
     in  subst body
	 handle SHARE => body
	      | Nth => ERRORty (* not enough arguments -- arity mismatch *)
     end
   | instantiateType (ERRORty,_) = ERRORty
   | instantiateType (ty,_) = ty

end (* abstype occ *)

local 
  exception CHECKEQ
in
fun checkEqTySig(ty, sign: polysign) =
    let fun eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(FLEXRECORDty(ref(CLOSED ty))) = eqty ty
	  | eqty(CONty(DEFtyc{tyfun,...}, args)) =
	      eqty(applyTyfun(tyfun,args))
	  | eqty(CONty(GENtyc{eq,...}, args)) =
	     (case !eq
		of OBJ => ()
		 | YES => app eqty args
		 | NO => raise CHECKEQ
		 | IND => raise CHECKEQ
		 | _ => impossible "checkEqTySig")
	  | eqty(VARty(ref(IBOUND n))) = 
	      let val {eq,...} = nth(sign,n)
	       in if eq then () else raise CHECKEQ
	      end
	  | eqty _ = ()
     in eqty ty;
	true
    end
    handle CHECKEQ => false
end

exception CompareTypes
fun compType(specty, specsign:polysign, actty,
	     actsign:polysign, actarity): unit =
    let val env = array(actarity,UNDEFty)
	fun comp(ty1, VARty(ref(INSTANTIATED(ty2)))) =
	      comp(ty1,ty2)
	  | comp(ty1, FLEXRECORDty(ref(CLOSED ty2))) = comp(ty1,ty2)
	  | comp(ty1, VARty(ref(IBOUND i))) =
	     (case env sub i
		of UNDEFty =>
		    (let val {weakness=aw,eq=ae} = nth(actsign,i)
		     in if aw < infinity
			then let fun checkweak(VARty(ref(IBOUND n))) =
					let val {weakness=sw,...} = nth(specsign,n)
					 in if sw > aw then raise CompareTypes
					    else ()
					end
				    | checkweak(CONty(_,args)) = app checkweak args
				    | checkweak _ = impossible "compType/checkweak"
			      in checkweak ty1
			     end
			else ();
			if ae andalso not(checkEqTySig(ty1,specsign))
			then raise CompareTypes
			else ();
			update(env,i,ty1)
		    end handle Nth => ())
		 | ty => if equalType(ty1,ty)
			 then ()
			 else raise CompareTypes)
	  | comp(ty1 as CONty(tycon, args), ty2 as CONty(tycon', args')) =
	      if eqTycon(tycon,tycon')
	      then app2 comp (args,args')
	      else (comp(reduceType ty1, ty2)
		    handle ReduceType =>
		      comp(ty1, reduceType ty2)
		      handle ReduceType => raise CompareTypes)
	  | comp(ty1 as CONty(DEFtyc _, _), ty2) = comp(reduceType ty1, ty2)
	  | comp(ty1, ty2 as CONty(DEFtyc _, _)) = comp(ty1, reduceType ty2)
	  | comp(_, ERRORty) = ()
	  | comp _ = raise CompareTypes
     in comp(specty,actty)
    end

fun compareTypes {spec : ty, actual: ty} : bool =
    let val actual = prune actual
     in case spec
	  of POLYty{sign,tyfun=TYFUN{body,...},...} =>
	      (case actual
		 of POLYty{sign=sign',tyfun=TYFUN{arity,body=body'},...} =>
		      (compType(body,sign,body',sign',arity); true)
		  | ERRORty => true
		  | _ => false)
	   | ERRORty => true
	   | _ =>
	      (case actual
		 of POLYty{sign,tyfun=TYFUN{arity,body},...} =>
		      (compType(spec,[],body,sign,arity); true)
		  | ERRORty => true
		  | _ => equalType(spec,actual))
    end handle CompareTypes => false

end (* structure TypesUtil *)
