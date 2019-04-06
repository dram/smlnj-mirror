(* sigmatch.sml *)

signature SIGMATCH =
sig
  structure Basics: BASICS
  val newsig : {t:int,s:int} -> Basics.Structure -> Basics.Structure
  val match : bool * Basics.Symbol.symbol list * Basics.Structure * Basics.Structure
	      -> Basics.Structure * Basics.thinning
  val realize : bool * Basics.Symbol.symbol list * Basics.stamp * Basics.Structure ->
	        Basics.Structure * Basics.trans list
  val applyFunctor : Basics.Functor * Basics.Structure * Basics.Symbol.symbol list -> 
	             Basics.Structure * Basics.thinning
end

structure SigMatch : SIGMATCH = struct

structure Basics = Basics

open List2 PrintUtil ErrorMsg Access Basics BareAbsyn BasicTypes
     EnvAccess EnvAccess.Env TypesUtil
open PrintType
val symName = Symbol.name
val anonName = Symbols.stringToSymbol "Anon"
fun for a b = app b a
fun arrayapp (f,a) =
    let fun loop i = (f(a sub i); loop(i+1))
     in loop 0 handle Subscript => ()
    end
fun arraymap (f,a) =
    let val b = array(length a, a sub 0) handle Subscript => a
	fun loop i = (update(b,i,f(a sub i)); loop(i+1))
     in loop 0 handle Subscript => b
    end


fun newsig {t=0,s=0} = (fn x=>x)
  | newsig {t=tycNameBase, s=strNameBase} =
    let fun newEnv{s,t} = {s=arraymap(newStr,s), t=arraymap(newTyc,t)}
	and newStr(str as STRstr{stamp,sign,table,env,kind}) =
	    if fixedStamp(stamp)
	    then str
	    else STRstr{stamp=stamp + strNameBase,
		        env=newEnv(env),
		        sign=sign,table=table,kind=kind}
          | newStr(INDstr i) = (print "INDstr in newsig/newStr: "; print i; newline();
			        impossible "newStr in newsig")
	and newTyc(tyc) =
	    (case tyc
	      of TYCON{stamp,kind=ABStyc,...} =>
		   if fixedStamp(stamp)
		   then tyc
		   else setTycStamp(stamp+tycNameBase,tyc)
	       | TYCON{stamp,kind=DATAtyc _,...} =>
		   if fixedStamp(stamp)
		   then tyc
		   else setTycStamp(stamp+tycNameBase,tyc)
	       | _ => tyc)
     in newStr
    end

exception CompareTypes
exception REFtyc
val refstamp = tycStamp(!refTycon)
and arraystamp = tycStamp(!arrayTycon)

fun compType(specty, specsign:polysign, actty, actsign:polysign, actarity): unit =
    let val env = array(actarity,UNDEFty)
	fun comp(ty1, VARty(ref(INSTANTIATED(ty2)))) =
	      comp(ty1,ty2)
	  | comp(ty1, FLEXRECORDty(ref(CLOSED ty2))) = comp(ty1,ty2)
	  | comp(ty1, VARty(ref(IBOUND i))) =
	     (case env sub i
		of UNDEFty =>
		    let val {weakness=aw,eq=ae} = nth(actsign,i)
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
			if ae 
			then checkEqTySig(ty1,specsign)
			     handle CHECKEQ => raise CompareTypes
			else ();
			update(env,i,ty1)
		    end
		 | ty => if equalType(ty1,ty)
			 then ()
			 else raise CompareTypes)
	  | comp(ty1 as CONty(ref tycon, args), ty2 as CONty(ref tycon', args')) =
	      if eqTycon(tycon,tycon')
	      then app2 comp (args,args')
	      else (comp(reduceType ty1, ty2)
		    handle ReduceType =>
		      comp(ty1, reduceType ty2)
		      handle ReduceType => raise CompareTypes)
	  | comp(_, ERRORty) = ()
	  | comp _ = raise CompareTypes
     in comp(specty,actty)
    end

fun compareTypes(spec: ty, actual: ty, name) : unit =
    let fun error() =
	    ((* openPervasives(); -- this was here because the pervasives
		would have been spliced out of the local structure env.  It
		should no longer be necessary with permeable structures *)
	     complain "value type in structure doesn't match signature spec";
	     PrintType.resetPrintType();
	     print ("  name: " ^ symName name ^ "\n  spec:   ");
	     PrintType.printType(spec);
	     print "\n  actual: ";
	     PrintType.printType(actual); newline())
     in case spec
	  of POLYty{sign,tyfun=TYFUN{body,...}} =>
	      (case actual
		 of POLYty{sign=sign',tyfun=TYFUN{arity,body=body'}} =>
		      (compType(body,sign,body',sign',arity)
		       handle CompareTypes => error())
		  | _ => error())
	   | ERRORty => ()
	   | _ =>
	      (case actual
		 of POLYty{sign,tyfun=TYFUN{arity,body}} =>
		      (compType(spec,[],body,sign,arity)
		       handle CompareTypes => error())
		  | _ => if equalType(spec,actual)
			 then ()
			 else error())
    end


(* signature matching *)

val defaultMapfns = 
    let fun ignore _ = ()
     in {mapstr=ignore,mapstr1=ignore,maptyc=ignore}
    end

fun sameSign(SOME(stamp1:int), SOME(stamp2:int)) = (stamp1=stamp2)
  | sameSign _ = false

fun abstract(sgn as STRstr{kind=SIGkind{stampcounts={s,t},...},...}, str) =
    let val strbase = genStrStamp()
	and tycbase = genTycStamp()
	fun abstractTyc(sigtyc,strtyc) =
	    case sigtyc
	      of TYCON{kind=DATAtyc _,...} => strtyc
	       | _ => let val stamp = tycStamp sigtyc
		       in if fixedStamp(stamp)
			  then strtyc
			  else setTycStamp(tycbase+stamp,sigtyc)
		      end
	fun abstractStr(STRstr{stamp,sign,table,env,...},
			str as STRstr{env=env',...}) =
	      if fixedStamp stamp
	      then str
	      else STRstr{stamp=strbase+stamp,env=abstractEnv(env,env'),
			  sign=sign,table=table,
			  kind=STRkind{path=[]}} (* ??? def of kind *)
	  | abstractStr (INDstr i,_) =
	      impossible ("3437 in sigmatch: " ^makestring i)
	  | abstractStr _ = impossible "9833 in sigmatch (abstractStr)"
	and abstractEnv({s=sSig,t=tSig}:strenv, {s=sStr,t=tStr}:strenv) =
	    let val sNew = array(length sSig, INDstr(~1))
		val tNew = array(length tSig, INDtyc [])
		fun foreachStr i =
		    (update(sNew,i,abstractStr(sSig sub i, sStr sub i));
		     foreachStr(i+1))
		fun foreachTyc i =
		    (update(tNew,i,abstractTyc(tSig sub i, tStr sub i));
		     foreachTyc(i+1))
	     in foreachStr 0
		handle Subscript =>
		foreachTyc 0
		handle Subscript =>
		{s=sNew,t=tNew}
	    end
     in strStampCount := !strStampCount + s;
	tycStampCount := !tycStampCount + s;
	abstractStr(sgn,str)
    end
  | abstract _ = impossible "8375 in sigmatch (abstract)"

fun match0 (mapfns as {mapstr,mapstr1,maptyc})
    	   (abs, path,
	    sgn as STRstr{stamp,sign,...},
	    str as STRstr{stamp=stamp',sign=sign',table,env,...})
	   : Structure * thinning =
      if fixedStamp(stamp) andalso stamp <> stamp'
      then condemn "fixed signature doesn't agree with structure"
      else if sameSign(sign,sign')
      then (mapstr(sgn,str);
	    (if abs then abstract(sgn,str) else str, NONE))
      else let val v = mkLvar()
	       val _ = (openStr(); openOld(([v],env),table))
	       val (str',transl) = realize0 mapfns (abs,path,stamp',sgn)
	    in closeStr(); (str',SOME(v,transl))
	   end
  | match0 _ _ = impossible "843 in sigmatch"

and realize0 (mapfns as {mapstr1,maptyc,mapstr})
	     (abs, path, strStamp,
	      sgn as STRstr{stamp=sigStamp,sign,table,
		     	    env = sigenv as {s=sSig,t=tSig},
			    kind=SIGkind{bindings,share,...},...})
	     : Structure * trans list =
    let val sNew = array(length sSig, INDstr(~1))
	val tNew = array(length tSig, INDtyc [])
		   (* dummy initial values, should never be referenced *)
	val newenv = {s=sNew,t=tNew}
        fun checkSpec spec =
	    case spec
	      of STRbind(STRvar{name,binding=INDstr i,...}) =>
		   let val STRvar{access,binding=str',...} =
			    lookSTRlocal name
			    handle Unbound =>
			      condemn("unmatched structure spec: " ^ symName name)
		       val (newStr,thin) = match0 mapfns
					    (false, name::path, sSig sub i, str')
		    in update(sNew,i,newStr);
		       [case thin
		          of NONE => VALtrans access
			   | SOME(v,transl) => THINtrans(access,v,transl)]
		   end
	       | TYCbind(ref(INDtyc [i])) =>
		  let val sigTycon = tSig sub i
		      val name = tycName sigTycon
		      val strTycon = !(lookTYClocal name) 
				     handle Unbound =>
				       condemn("unmatched type spec: "^
					        symName(name))
		      val s = tycStamp sigTycon
		      val s' = tycStamp strTycon
		   in update(tNew,i,strTycon);
		      if fixedStamp s andalso s <> s'
		      then if equalTycon(sigTycon,strTycon)
			   then (maptyc(s,strTycon); nil)
			   else condemn("bad match for fixed type spec "
				        ^ symName(name))
		      else (case (sigTycon, strTycon)
			     of (TYCON{arity,kind=DATAtyc dcons,...},
				 TYCON{arity=arity',kind=DATAtyc dcons',...}) =>
				  if arity = arity'
				     andalso length(dcons) = length(dcons')
				  then (maptyc(s,strTycon); nil)
				  else condemn("mismatching datatype spec: "
					       ^ symName(name))
			      | (TYCON{kind=DATAtyc _,...}, _) => 
				  condemn("unmatched datatype spec: "^symName(name))
			      | (TYCON{arity,kind=ABStyc,eq,...}, _) =>
				  if arity <> tyconArity(strTycon)
				  then condemn("mismatching tycon arities: "
					       ^ symName(name))
				  else if (!eq=YES) andalso not(isEqTycon(strTycon))
				  then condemn("mismatched eqtype spec: "
					       ^ symName(name))
				  else (maptyc(s,strTycon); nil)
			      | _ => impossible "realize0/checkSpec/TYCbind")
		   end
	       | CONbind(DATACON{name,typ,rep=VARIABLE _,const,...}) =>
		   let val DATACON{typ=typ',rep=VARIABLE(access),...} =
			     lookEXNlocal name
			     handle Unbound =>
			      condemn ("unmatched exception spec: "^symName(name))
		    in compareTypes(typeInContext(typ,newenv),typ',name);
		       [VALtrans access]
		   end
	       | CONbind(DATACON{name,typ,...}) =>
		   let val DATACON{typ=typ',...} =
			     lookCONlocal name
			     handle Unbound =>
			      condemn ("unmatched data constructor spec: "
				       ^symName(name))
		    in compareTypes(typeInContext(typ,newenv),typ',name); nil
		   end
	       | VARbind(VALvar{name,vtype,...}) =>
		  (case (lookVARCONlocal name
			  handle Unbound =>
			    condemn("unmatched val spec: "^symName(name)))
		     of VARbind(VALvar{access,vtype=vtype',...}) =>
			  (* no propagation of INLINE access!! *)
			 (compareTypes(typeInContext(!vtype,newenv),!vtype',name);
 			  [case access of INLINE _ => VALtrans access
					| PATH _ => VALtrans access
					| LVAR _  => 
					   if !ErrorMsg.anyErrors
					   then VALtrans access
					   else impossible "sigmatch.1"
					| _ => impossible "sigmatch.2"])
		      | CONbind(dcon as DATACON{typ=typ',...}) =>
			  (compareTypes(typeInContext(!vtype,newenv),typ',name);
			   [CONtrans dcon])
		      | _ => impossible "sigmatch.476")
	       | _ => impossible "sigMatch--unexpected spec. "
        fun checkList (a::rest) =
	      (checkSpec a handle Syntax => nil) @ checkList rest
          | checkList nil =
	      if !ErrorMsg.anyErrors then raise Syntax else nil

	val trans = checkList bindings
	val _ = Sharing.checkSharing(table,newenv,share)
        val str0 = STRstr{stamp=strStamp,sign=sign,table=table,env=newenv,
		  	  kind=STRkind{path=path}}
	val str = if abs then abstract(sgn,str0) else str0
     in mapstr1(sigStamp,str);
	(str, trans)
    end
  | realize0 _ _ = impossible "783 in sigmatch"

val match = match0 defaultMapfns
and realize = realize0 defaultMapfns

fun applyFunctor(FUNCTOR{paramName,param,tycCount,body},arg,path)
     : Structure * thinning =
    let val STRstr{kind=SIGkind{stampcounts={s=sCount,t=tCount},...},...} = param
	val tycmap = array(tycCount,INDtyc [])
	val strmap = array(sCount,INDstr(~1))
	fun maptyc(stamp,tycon) =
	    if fixedStamp stamp then () else update(tycmap,stamp,tycon)
	fun mapstr1(stamp,str) =
	    if fixedStamp stamp then () else update(strmap,stamp,str)
	fun mapstr(sgn as STRstr{stamp,env={s,t},kind=SIGkind{bindings,...},...},
		   str as STRstr{table,env={s=s',t=t'},...}) =
	    let fun mapbinding(STRbind(STRvar{name,binding=INDstr i,...})) =
		      let val sgn' = s sub i
			  val str' = let val STRvar{binding,...} = 
					       lookSTRinTable(table,name)
				      in case binding
					   of INDstr j => s' sub j
					    | st => st
				     end
		       in mapstr(sgn',str')
		      end
		  | mapbinding(TYCbind(ref(INDtyc [i]))) =
		      let val tycon = t sub i
			  val name = tycName tycon
			  val tycon' = case !(lookTYCinTable(table,name))
					 of INDtyc [j] => t' sub j
					  | tyc => tyc
		       in maptyc(tycStamp tycon,tycon')
		      end
		  | mapbinding _ = ()
	     in if fixedStamp stamp then () 
		else (update(strmap,stamp,str);
		      for bindings mapbinding)
	    end
          | mapstr _ = impossible "887 in sigmatch"
	fun insttyc(tyc) =
	    case tyc
	      of TYCON{stamp,arity,eq,path=path',kind} =>
		   if stamp < tCount  (* param *)
		   then tycmap sub stamp
		   else if stamp < stampBase  (* generative *)
		   then (case tycmap sub stamp
			  of INDtyc [] =>
			     let val newtyc =
				  TYCON{stamp = genTycStamp(),
					eq = if !eq = YES then eq else ref MAYBE,
					arity = arity,
					path = path'@path,
					kind = case kind
						of DEFtyc(TYFUN{arity,body}) =>
						    DEFtyc(TYFUN{arity=arity,
								 body=insttype body})
						 | _ => kind}
			      in update(tycmap,stamp,newtyc);
				 newtyc
			     end
			   | tyc' => tyc')
		   else tyc
	       | _ => impossible "SigMatch.applyFunctor/insttyc"
	and insttype(ty) =
	    case ty
	      of CONty(ref tycon,args) =>
		   CONty(ref(insttyc tycon),map insttype args)
	       | _ => ty
	fun inststr(str as STRstr{stamp,sign,table,env,kind}) =
	    if stamp < sCount  (* parameter formal *)
	    then let val str' as STRstr{stamp=stamp',sign=sign',...}
			 = strmap sub stamp
		  in if sameSign(sign,sign')
		     then str'
		     else STRstr{stamp=stamp',sign=sign,table=table,
				 env=instenv(env),
			  	 kind= case kind
				         of STRkind _ => kind
					  | _ => STRkind{path=[]}}
		 end
	    else if stamp < stampBase  (* generative formal *)
	    then let val STRkind{path=path'} = kind
		  in mkSTR(path'@path,sign,table,instenv(env))
		 end
	    else str  (* absolute *)
	  | inststr (INDstr i) = impossible ("8484 in sigmatch: "^makestring i)
	and instenv({s,t}:strenv) =
	    {s=arraymap(inststr,s),t=arraymap(insttyc,t)}
        fun checkeqstr(STRstr{env as {s,t},...}) =
	    (arrayapp(defineEqTycon(tyconInContext env), t);
	     arrayapp(checkeqstr,s))
	val (_,thin) = match0 {mapstr=mapstr,mapstr1=mapstr1,maptyc=maptyc}
			      (false,[paramName],param,arg)
	    (* discarding the instantiated parameter structure *)
	val str = inststr body
     in checkeqstr str;
	(str, thin)
    end

end (* structure SigMatch *)
