(* sigmatch.sml *)

signature SIGMATCH =
sig
  structure Basics: BASICS
  val setParent : Basics.Structure -> Basics.Structure -> unit
  val linkParents : Basics.Structure -> unit
  val newsig : Stamp.state * Basics.Structure -> Basics.Structure
  val match : bool * Basics.Symbol.symbol list * Stamp.state * Basics.Structure
              * Basics.Structure -> Basics.Structure * Basics.thinning
  val realize : bool * Basics.Symbol.symbol list * Stamp.state * Basics.stamp
                * Basics.Structure * Basics.Structure
		-> Basics.Structure * Basics.trans list
  val applyFunctor : Basics.Functor * Basics.Structure * Basics.Symbol.symbol list 
		     * Stamp.state -> Basics.Structure * Basics.thinning
end

structure SigMatch : SIGMATCH = struct

structure Basics = Basics

open List2 PrintUtil ErrorMsg Access Basics BareAbsyn BasicTypes
     EnvAccess EnvAccess.Env TypesUtil PrintType

val symName = Symbol.name
val anonName = Symbol.symbol "Anon"
fun for a b = app b a


fun setParent (parent: Structure) =
    fn (STRstr{env={s,...},...}) =>
	((case s sub 0
	   of NULLstr => update(s,0,parent)
	    | _ => ())
	 handle Subscript => ())
     | _ => impossible "SigMatch.setParent"

fun linkParents(str as STRstr{env={s,...},...}) =
    ArrayExt.app((fn str' => (setParent str str'; linkParents str')), s, 1)


fun newsig (stamps, sg as STRstr{kind=SIGkind{stampcounts,...},...}) =
    let val base as (tycBase,strBase) = Stamp.current(stamps)
	fun newEnv{s,t} = {s=ArrayExt.map(newStr,s,1), t=ArrayExt.map(newTyc,t,0)}
	and newStr(str as STRstr{stamp,sign,table,env,kind}) =
	    if Stamp.fixed(stamp)
	    then str
	    else let val newenv as {s,t} = newEnv(env)
		     val new = STRstr{stamp=stamp + strBase,
				      env=newenv,
				      sign=sign,table=table,kind=kind}
		  in ArrayExt.app((setParent new), s, 1);
		     new
		 end
          | newStr(INDstr i) = (print "INDstr in newsig/newStr: "; print i; newline();
			        impossible "newStr in newsig")
	and newTyc(tyc) =
	    (case tyc
	      of TYCON{stamp,kind=ABStyc,...} =>
		   if Stamp.fixed(stamp)
		   then tyc
		   else setTycStamp(stamp+tycBase,tyc)
	       | TYCON{stamp,kind=DATAtyc _,...} =>
		   if Stamp.fixed(stamp)
		   then tyc
		   else setTycStamp(stamp+tycBase,tyc)
	       | _ => tyc)
     in Stamp.move(stamps,stampcounts);
	case base
	  of (0,0) => sg
	   | _ => newStr sg
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
	    (complain "value type in structure doesn't match signature spec";
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

fun abstract(sgn as STRstr{kind=SIGkind{stampcounts,...},...}, str, stamps) =
    let val (tycbase,strbase) = Stamp.current(stamps)
	fun abstractTyc(sigtyc,strtyc) =
	    case sigtyc
	      of TYCON{kind=DATAtyc _,...} => strtyc
	       | _ => let val stamp = tycStamp sigtyc
		       in if Stamp.fixed(stamp)
			  then strtyc
			  else setTycStamp(tycbase+stamp,sigtyc)
		      end
	fun abstractStr(STRstr{stamp,sign,table,env,...},
			str as STRstr{env=env',...}) =
	      if Stamp.fixed stamp
	      then str
	      else let val newenv as {s,t} = abstractEnv(env,env')
		       val newstr = STRstr{stamp=strbase+stamp,
				    	   env=newenv,
					   sign=sign,table=table,
					   kind=STRkind{path=[]}} (* ??? def of kind *)
		    in ArrayExt.app((setParent newstr), s, 1);
		       newstr
		   end
	  | abstractStr (INDstr i,_) =
	      impossible ("3437 in sigmatch: " ^makestring i)
	  | abstractStr _ = impossible "9833 in sigmatch (abstractStr)"
	and abstractEnv({s=sSig,t=tSig}:strenv, {s=sStr,t=tStr}:strenv) =
	    let val sNew = array(Array.length sSig, NULLstr)
		val tNew = array(Array.length tSig, NULLtyc)
		fun foreachStr i =
		    (update(sNew,i,abstractStr(sSig sub i, sStr sub i));
		     foreachStr(i+1))
		fun foreachTyc i =
		    (update(tNew,i,abstractTyc(tSig sub i, tStr sub i));
		     foreachTyc(i+1))
	     in foreachStr 1
		handle Subscript =>
		foreachTyc 0
		handle Subscript =>
		{s=sNew,t=tNew}
	    end
     in Stamp.move(stamps,stampcounts);
	abstractStr(sgn,str)
    end
  | abstract _ = impossible "8375 in sigmatch (abstract)"

fun match0 (mapfns as {mapstr,mapstr1,maptyc})
    	   (abs, path, stamps,
	    sgn as STRstr{stamp,sign,...},
	    str as STRstr{stamp=stamp',sign=sign',table,env,...})
	   : Structure * thinning =
      if Stamp.fixed(stamp) andalso stamp <> stamp'
      then condemn "fixed signature doesn't agree with structure"
      else if sign = sign'
      then (mapstr(sgn,str);
	    (if abs then abstract(sgn,str,stamps) else str, NONE))
      else let val v = mkLvar()
	       val _ = (openStr(); openOld({path=[v],strenv=env},table))
	       val (str',transl) = realize0 mapfns (abs,path,stamps,stamp',sgn,NULLstr)
	    in closeStr(); (str',SOME(v,transl))
	   end
  | match0 _ _ = impossible "843 in sigmatch"

and realize0 (mapfns as {mapstr1,maptyc,mapstr})
	     (abs, path, stamps, strStamp,
	      sgn as STRstr{stamp = boundStamp, sign, table,
		     	    env = sigenv as {s=sSig,t=tSig},
			    kind = SIGkind{bindings,share,...},...},
	      parent)
	     : Structure * trans list =
    let val sNew = array(Array.length sSig, NULLstr)
	val tNew = array(Array.length tSig, NULLtyc)
		   (* dummy initial values, should never be referenced *)
	val _ = update(sNew,0,parent) handle Subscript => ()
		(* handler for case of empty parameter *)
	val newenv = {s=sNew,t=tNew}
        fun checkSpec spec =
	    case spec
	      of STRbind(STRvar{name,binding=INDstr i,...}) =>
		   let val STRvar{access,binding=str',...} =
			    lookSTRlocal name
			    handle Unbound =>
			      condemn("unmatched structure spec: " ^ symName name)
		       val (newStr,thin) = match0 mapfns
					    (false, name::path, stamps, sSig sub i, str')
		    in update(sNew,i,newStr);
		       [case thin
		          of NONE => VALtrans access
			   | SOME(v,transl) => THINtrans(access,v,transl)]
		   end
	       | TYCbind(ref(INDtyc i)) =>
		  let val sigTycon = tSig sub i
		      val name = tycName sigTycon
		      val strTycon = !(lookTYClocal name) 
				     handle Unbound =>
				       condemn("unmatched type spec: "^
					        symName(name))
		      val s = tycStamp sigTycon
		      val s' = tycStamp strTycon
		   in update(tNew,i,strTycon);
		      if Stamp.fixed s andalso s <> s'
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
			     lookCONlocal name
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
	val str = if abs then abstract(sgn,str0,stamps) else str0
     in mapstr1(boundStamp,str);
	linkParents str;
	(str, trans)
    end
  | realize0 _ _ = impossible "783 in sigmatch"

val match = match0 defaultMapfns
and realize = realize0 defaultMapfns

fun applyFunctor(FUNCTOR{paramName,param,tycCount,body},arg,path,stamps)
     : Structure * thinning =
    let val STRstr{kind=SIGkind{stampcounts=(tCount,sCount),...},...} = param
	val tycmap = array(tycCount,NULLtyc)
	val strmap = array(sCount,NULLstr)
	fun maptyc(stamp,tycon) =
	    if Stamp.fixed stamp then () else update(tycmap,stamp,tycon)
	fun mapstr1(stamp,str) =
	    if Stamp.fixed stamp then () else update(strmap,stamp,str)
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
		  | mapbinding(TYCbind(ref(INDtyc i))) =
		      let val tycon = t sub i
			  val name = tycName tycon
			  val tycon' = case !(lookTYCinTable(table,name))
					 of INDtyc j => t' sub j
					  | tyc => tyc
		       in maptyc(tycStamp tycon,tycon')
		      end
		  | mapbinding _ = ()
	     in if Stamp.fixed stamp then () 
		else (update(strmap,stamp,str);
		      for bindings mapbinding)
	    end
          | mapstr _ = impossible "887 in sigmatch"

	val deftycs = ref([]: int list list)

	fun insttyc(tyc,loc) =
	    case tyc
	      of TYCON{stamp,arity,eq,path=path',kind} =>
		   if stamp < tCount  (* param *)
		   then tycmap sub stamp
		   else if Stamp.bound(stamp)  (* generative *)
		   then (case tycmap sub stamp
			  of NULLtyc =>
			     (case kind
			        of DEFtyc _ =>
				   (deftycs := (rev loc) :: !deftycs;
				    tyc)
				 | _ =>
				   let val newtyc =
					   TYCON{stamp = Stamp.newTyc(stamps),
					   eq = if !eq = YES then eq else ref MAYBE,
					   arity = arity,
					   path = path'@path,
					   kind = kind}
			            in update(tycmap,stamp,newtyc);
				       newtyc
			           end)
			   | tyc' => tyc')
		   else tyc
	       | _ => (print "insttyc: "; PrintType.printTycon tyc; print "\n";
		       impossible "SigMatch.applyFunctor/insttyc")

	fun inststr(str as STRstr{stamp,sign,table,env,kind},loc) =
	    if stamp < sCount  (* parameter formal *)
	    then let val str' as STRstr{stamp=stamp',sign=sign',...}
			 = strmap sub stamp
		  in if sign = sign'
		     then str'
		     else STRstr{stamp=stamp',sign=sign,table=table,
				 env=instenv(env,loc),
			  	 kind= case kind
				         of STRkind _ => kind
					  | _ => STRkind{path=[]}}
		 end
	    else if Stamp.bound(stamp)  (* generative *)
	    then let val STRkind{path=path'} = kind
		  in STRstr{stamp = Stamp.newStr(stamps),
			    sign = sign,
			    table = table,
			    env = instenv(env,loc),
			    kind = STRkind{path=path'@path}}
		 end
	    else str  (* absolute *)
	  | inststr (INDstr i,_) = impossible ("SigMatch.applyFunctor.instsr: "
					       ^makestring i)

	and instenv({s,t}: strenv, loc: int list) : strenv =
	    let fun mapi (f,a,start,loc) =
		  let val b = array(Array.length a, a sub 0) handle Subscript => a
		      fun loop i = (update(b,i,f(a sub i,i::loc)); loop(i+1))
		   in loop start handle Subscript => b
		  end
	     in {s=mapi(inststr,s,1,loc),t=mapi(insttyc,t,0,loc)}
	    end

	fun instloc (env: strenv) (loc: int list) : tycon =
	    let fun lookTycPath([i],env') = (i,env')
		  | lookTycPath(i::rest,{s,...}) =
		      (case s sub i
			of STRstr{env,...} => lookTycPath(rest,env)
			 | FCTstr(STRstr{env,...}) => lookTycPath(tl rest, env)
			      (* i = 0 and hd rest = 1 *)
			 | _ => ErrorMsg.impossible "SigMatch.applyFunctor 1")
		  | lookTycPath _ = ErrorMsg.impossible "SigMatch.applyFunctor 2"
		val (i,env' as {s,t}) = lookTycPath(loc,env)
	     in case (t sub i)
	         of tyc as TYCON{stamp,arity,eq,path=path',kind} =>
		    if Stamp.bound stamp then
		        case tycmap sub stamp
		        of NULLtyc =>
			   let val newtyc =
				   TYCON{stamp = Stamp.newTyc(stamps),
				         eq = if !eq = YES then eq else ref MAYBE,
					 arity = arity,
					 path = path'@path,
					 kind = 
					   case kind
					     of DEFtyc(TYFUN{arity,body}) =>
						DEFtyc(TYFUN{arity=arity,
							     body=instdef env' body})
					      | _ => ErrorMsg.impossible "SigMatch 4"}
			    in update(tycmap,stamp,newtyc);
			       update(t,i,newtyc);
			       newtyc
			   end
		       | tyc' => tyc'
		      else tyc
		  | _ => ErrorMsg.impossible "SigMatch.applyFunctor 3"
	    end

        and instTycInDef env tycon =
	    case tycon
	      of TYCON _ => tycon (* fixed stamp assumed *)
	       | RELtyc loc => instloc env loc  (* bound tycon *)
	       | _ => ErrorMsg.impossible "SigMatch.applyFunctor 5"

	and instdef env ty =
	    case ty
	      of CONty(ref tycon,args) =>
		   CONty(ref(instTycInDef env tycon),map (instdef env) args)
	       | _ => ty

	fun instDEFtycs(locs,env) = app (instloc env) locs

        fun checkeqstr(STRstr{env as {s,t},...}) =
	    (ArrayExt.app(defineEqTycon(tyconInContext env), t, 0);
	     ArrayExt.app(checkeqstr,s,1))
          | checkeqstr(INDstr n) = impossible "SigMatch.applyFunctor.checkeqstr"


	val (argstr,thin) = match0 {mapstr=mapstr,mapstr1=mapstr1,maptyc=maptyc}
			      (false,[paramName],stamps,param,arg)
	    (* argstr is the instantiated parameter structure *)


	val str as STRstr{env as {s,...},...} = inststr(body,[])


     in linkParents str;
	instDEFtycs(!deftycs,env);
	checkeqstr str;
	(case (s sub 0)
	 of FCTstr _ => update(s,0,FCTstr argstr)
	  | _ => ()) handle Subscript => ();
	    (* handler for empty parameter case *)
	(str, thin)
    end

end (* structure SigMatch *)
