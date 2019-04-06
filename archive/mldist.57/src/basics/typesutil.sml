(* Copyright 1989 by AT&T Bell Laboratories *)
(* typesutil.sml *)

structure TypesUtil : TYPESUTIL = struct

structure Basics = Basics

open PrintUtil Basics List2 ErrorMsg BasicTypes

(* primitive operations on tycons *)

fun tycStamp(tycon) = raise Cascade "tycStamp"

fun tycName(tycon) =	(* fix this some day to return entire path *)
    case tycon
      of GENtyc{path=name::_,...} => name
       | DEFtyc{path=name::_,...} => name
       | _ => impossible "tycName"

fun tyconArity(GENtyc{arity,...}) = arity
  | tyconArity(RECORDtyc l) = length l
  | tyconArity(DEFtyc{tyfun=TYFUN{arity,...},...}) = arity
  | tyconArity _ = impossible "tyconArity"

fun tycPath (GENtyc{path,...}) = path
  | tycPath (DEFtyc{path,...}) = path
  | tycPath _ = impossible "tycPath"

fun setTycPath(tycon,path) =
    case tycon
      of GENtyc{stamp,arity,eq,kind,...} =>
	   GENtyc{stamp=stamp,path=path,arity=arity,eq=eq,kind=kind}
       | DEFtyc{tyfun,...} => DEFtyc{tyfun=tyfun,path=path}
       | _ => impossible "setTycName"

fun eqTycon(GENtyc{stamp=s,...},GENtyc{stamp=s',...}) = s=s'
  | eqTycon(RECORDtyc l1, RECORDtyc l2) = l1=l2
  | eqTycon(t1,t2) = t1=t2

fun getEpath([],str) = str
  | getEpath(i::r,STRstr{env=REL{s,...},...}) = getEpath(r,s sub i)
  | getEpath _ = impossible "getEpath"

fun getEpathTyc([i],REL{t,...}) = t sub i
  | getEpathTyc(i::r,REL{s,...}) =
    let val STRstr{env,...} = s sub i
     in getEpathTyc(r,env)
    end
  | getEpathTyc _ = impossible "getEpathTyc"

(* lookTycPath: int list * strenv -> tycon
     interpret index path through structure environments *)
fun lookTycPath([i],REL{s,t}) = ((t sub i)
handle Subscript => (print "$$ lookTycPath 1: "; print i; print " "; 
print(Array.length t); print "\n"; raise Subscript))
(*fun lookTycPath([i],REL{s,t}) = t sub i*)
  | lookTycPath(i::rest,REL{s,t}) =
      (case (s sub i
handle Subscript => (print "$$ lookTycPath 2: "; prIntPath(i::rest); print "\n";
		     raise Subscript))
(*      (case s sub i *)
        of STRstr{env,...} => lookTycPath(rest,env)
	 | NULLstr => (prIntPath (i::rest); newline();
		       ErrorMsg.impossible "TypesUtil.lookTycPath: NULLstr")
	 | INDstr _ => (prIntPath (i::rest); newline();
		        ErrorMsg.impossible "TypesUtil.lookTycPath: INDstr")
	 | SHRstr _ => (prIntPath (i::rest); newline();
		        ErrorMsg.impossible "TypesUtil.lookTycPath: SHRstr"))
  | lookTycPath _ = raise ErrorMsg.Cascade "TypesUtil.lookTycPath.2"

fun tyconInContext env =
    fn (RELtyc path) => (lookTycPath(path,env)
 handle Subscript => (print "tyconInContext: "; prIntPath path; print "\n";
 raise Subscript))
     | (INDtyc i) => 
         (case env of REL{t,...} => t sub i | _ => impossible "tyconInContext")
     | (SHRtyc p) => getEpathTyc(p,env)
     | tyc => tyc

fun printableTycon path (STRstr{env as REL{s,t},kind=SIGkind{bindings,...},...}) =
  (fn (RELtyc (p as [_])) => let val tyc = lookTycPath(p,env)
			      in setTycPath(tyc, tycPath tyc @ path)
			     end
    | (RELtyc(i::rest)) => 
      let fun scan(STRbind(STRvar{name,binding=INDstr j,...}):: others) =
		 if i=j then printableTycon (name@path) (s sub i) (RELtyc rest)
			else scan others
	    | scan(_::others) = scan others
	    | scan [] = impossible "TypesUtil.printableTycon 1"
       in scan bindings
      end
    | (INDtyc _) => impossible "TypesUtil.printableTycon: INDtyc"
    | (SHRtyc _) => impossible "TypesUtil.printableTycon: SHRtyc"
    | tyc => tyc)
  | printableTycon _ _ = impossible "printableTycon - bad arg"

(* relativize type to structure context, represented by the strenv *)
(* could optimize to share; should merge with applyPoly ??? *)
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

fun typeInContext(ty,DIR) = ty
  | typeInContext(ty,env) = rewriteType (tyconInContext env) ty

fun printableType str = rewriteType (printableTycon nil str)

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

fun mkUBOUND(id : Symbol.symbol) : tvkind =
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
        val weakness = if Ascii.isDigit(ordof(s,start)) handle Ord => false
		         then digits(start,0)
		         else infinity
     in UBOUND{name=Symbol.symbol s,weakness=weakness,eq=eq}
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
	      else (equalType(reduceType ty, ty')
		    handle ReduceType =>
		      (equalType(ty,reduceType ty')
		       handle ReduceType => false))
	  | eq(ERRORty,_) = true
	  | eq(_,ERRORty) = true
	  | eq _ = false
     in eq(prune ty, prune ty')
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

(* this should be merged with typeInContext and used in varApplied, etc. *)
fun applyPoly(POLYty{sign,tyfun,abs}, absocc: int, wmax: int) : ty =
      applyTyfun(tyfun,
      		 map (fn {weakness,eq} => 
		       VARty(ref(META{weakness = if weakness >= infinity
					         then infinity
						 else min(weakness+absocc-abs,wmax),
				      depth = infinity,
				      eq = eq})))
		     sign)
  | applyPoly(ty,_,_) = ty


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
    | FLEXRECORDty(ref(OPEN l)) => app (compressTy o #2) l
    | POLYty{tyfun=TYFUN{body,...},...} => compressTy body
    | _ => ()

end (* structure TypesUtil *)
