(* sigmatch.sml *)

structure SigMatch = struct

local 
  open List2 PrintUtil ErrorMsg Access Basics BareAbsyn BasicTypes
       EnvAccess EnvAccess.Env TypesUtil
  open PrintType
  val symName = Symbol.name
in

fun for a b = app b a

fun arraymap (f,a) =
    let val b = array(length a, a sub 0) handle Subscript => a
        fun loop i = (update(b,i,f(a sub i)); loop(i+1))
     in loop 0 handle Subscript => b
    end

fun boundStamp stamp = stamp < 10000

fun (* newsig {t=0,s=0} = (fn x=>x)
  | *) newsig {t=tycNameBase, s=strNameBase} =
    let fun newEnv{s,t} = {s=arraymap(newStr,s), t=arraymap(newTyc,t)}
	and newStr(str as STRstr{stamp,sign,table,env,kind}) =
	    if boundStamp(stamp)
	    then STRstr{stamp=stamp + strNameBase,
		        env=newEnv(env),
		        sign=sign,table=table,kind=kind}
	    else str
	  | newStr _ = impossible "8123 in sigmatch"
	and newTyc(tyc) =
	    (case tyc
	      of VARtyc{stamp,name,arity} => (* VARtyc always bound *)
		   VARtyc{stamp=stamp+tycNameBase,name=name,arity=arity}
	       | DATAtyc{stamp,name,params,dcons} =>
		   if boundStamp(stamp)
		   then DATAtyc{stamp=stamp+tycNameBase,name=name,
				    params=params,dcons=dcons}
		   else tyc
	       | _ => tyc)
     in newStr
    end

exception CompareTypes

fun compareTypes(spec: ty, senv, actual: ty, name) : unit =
    let val env = ref []
	exception Lookup
	fun lookup(tyv,env) =
	    case env
	      of [] => raise Lookup
	       | (tyv',ty)::rest => 
		   if eqTyvar(tyv,tyv') then ty else lookup(tyv,rest)
        fun bind(tyv,ty) = env := (tyv,ty)::(!env)
	fun comp1(ty1,ty2) =
	    case (ty1,ty2)
	      of (VARty tyv1,VARty tyv2) => 
		    if eqTyvar(tyv1,tyv2) then () else raise CompareTypes
	       | (CONty(ref tycon1, args1), CONty(ref tycon2, args2)) =>
		   if eqTyconRel(tycon1,senv,tycon2,senv)
		     then app2 comp1 (args1,args2)
		     else let val tycon1 = tyconInContext(tycon1,senv)
			      and tycon2 = tyconInContext(tycon2,senv)
			      val ty1 = CONty(ref tycon1,args1)
			      val ty2 = CONty(ref tycon2,args2)
			   in (case (tycon1,tycon2)
			         of (TYPEtyc _, _) => comp1(expandTy(ty1),ty2)
				  | (_, TYPEtyc _) => comp1(ty1,expandTy(ty2))
				  | _ => raise CompareTypes)
			  end
	       | _ => raise CompareTypes
	fun comp(spec,actual) =
	    case (spec,actual)
	      of (_, VARty(TYVAR{status=ref(INSTANTIATED(ty)),...})) =>
		   comp(spec,ty)
	       | (_, VARty(tyv)) =>
		   (comp1(spec, lookup(tyv,!env))
		    handle Lookup => bind(tyv, spec))
	       | (CONty(ref tycon, args), CONty(ref tycon', args')) =>
		   let val tycon = tyconInContext(tycon,senv)
		    in if eqTycon(tycon,tycon')
		       then app2 comp (args,args')
		       else (case (tycon, tycon')
			       of (TYPEtyc _, _) => 
				    comp(expandTy(CONty(ref tycon,args)), actual)
			        | (_,TYPEtyc _) => comp(spec, expandTy actual)
				| _ => raise CompareTypes)
		   end
	        | (_, UNKNOWNty) => ()
		| _ => raise CompareTypes
     in comp(spec,actual)
	handle CompareTypes =>
		    (complain "Type in structure doesn't match signature";
		     print ("name = " ^ symName name ^ "\nspec = ");
		     PrintType.printType(spec); print "\nactual = ";
		     PrintType.printType(actual); newline())
    end


(* signature matching *)

val defaultMapfns = 
    let fun ignore _ = ()
     in {mapstr=ignore,mapstr1=ignore,maptyc=ignore}
    end

fun sameSign(SOME(stamp1:int), SOME(stamp2:int)) = (stamp1=stamp2)
  | sameSign _ = false

fun sigMatch(abs, mapfns as {mapstr,mapstr1,maptyc},
    	     sgn as STRstr{stamp,sign,...},
	     str as STRstr{stamp=stamp',sign=sign',table,env,...})
	   : Structure * thinning =
    let fun insttyc(sigtyc,strtyc) =
	    case sigtyc
	      of DATAtyc _ => strtyc
	       | _ => if fixedStamp(tycStamp sigtyc)
		      then strtyc
		      else setTycStamp(!genTycStamp(),sigtyc)
	fun inststr(sigstr as STRstr{stamp,sign,table,env,kind},
		    strstr as STRstr{env=env',...}) =
	      if fixedStamp stamp
	      then strstr
	      else STRstr{stamp= !genStrStamp(),env=instenv(env,env'),
			  sign=sign,table=table,kind=kind}
	  | inststr (INDstr i,_) = impossible ("3437 in sigmatch: "^makestring i)
	and instenv({s,t}:strenv,{s=sStr,t=tStr}) =
	    let val sNew = array(length s, INDstr(~1))
	    	val tNew = array(length t, INDtyc [])
		fun foreachStr i =
		    (update(sNew,i,inststr(s sub i, sStr sub i)); foreachStr(i+1))
		fun foreachTyc i =
		    (update(tNew,i,insttyc(t sub i, tStr sub i)); foreachTyc(i+1))
	     in foreachStr 0
		handle Subscript =>
		foreachTyc 0
		handle Subscript =>
		{s=sNew,t=tNew}
	    end
     in if fixedStamp(stamp) andalso stamp <> stamp'
	  then condemn "sigMatch: fixed signature doesn't agree with structure"
	else if sameSign(sign,sign')
	  then (mapstr(sgn,str);
	        (if abs then inststr(sgn,str) else str,NONE))
	else let val v = mkLvar()
		 val mark1 = mark() before (openStr(); openOld(([v],env),table))
		 val (str',transl) = sigFill(abs,mapfns,stamp',sgn)
	      in close mark1; (str',SOME(v,transl))
	     end
    end
  | sigMatch _ = impossible "843 in sigmatch"

and sigFill(abs,mapfns as {mapstr1,maptyc,mapstr},strStamp,
	    STRstr{stamp=sigStamp,sign,table,env = {s=sSig,t=tSig},
	    	   kind=SIGkind{bindings,share,...},...})
	   : Structure * trans list =
    let val sNew = array(length sSig, INDstr(~1))
	val tNew = array(length tSig, INDtyc [])
		   (* dummy initial values, should never be referenced *)
	val newenv = {s=sNew,t=tNew}
        fun checkSpec spec =
	    case spec
	      of STRbind(STRvar{name,binding=INDstr i,...}) =>
		  (let val STRvar{access,binding=str',...} = lookSTR name
		       val (newStr,thin) = sigMatch(abs,mapfns, sSig sub i, str')
		    in update(sNew,i,newStr);
		       [case thin
		          of NONE => VALtrans access
			   | SOME(v,transl) => THINtrans(access,v,transl)]
		   end
		   handle Table.Notfound_Table =>
		     condemn("sigMatch: missing structure " ^ symName name))
	       | TYCbind(ref(INDtyc [i])) =>
		 (let val sigTycon = tSig sub i
		      val name = tycName sigTycon
		      val strTycon = !(lookTYC name) 
				     handle Table.Notfound_Table =>
				       condemn ("sigMatch: missing type "^symName(name))
		      val s = tycStamp sigTycon		      
		   in if fixedStamp s andalso s <> tycStamp strTycon
		      then condemn "sigMatch: fixed type constructor spec does't match"
		      else (case (sigTycon, strTycon)
			     of (DATAtyc{params,dcons,...},
				 DATAtyc{params=params',dcons=dcons',...}) =>
				  if length params = length params'
				      andalso length(!dcons) = length(!dcons')
				  then (update(tNew,i,strTycon);
					maptyc(s,strTycon); nil)
				  else condemn "sigMatch: datatype mismatch"
			      | (DATAtyc _, _) => 
				  condemn("sigMatch: datatype expected - "^symName(name))
			      | _ =>      
				  if tyconArity(sigTycon) = tyconArity(strTycon)
				  then (update(tNew,i,strTycon);
					maptyc(s,strTycon); nil)
				  else condemn "sigMatch: tycon arity")
		   end)
	       | CONbind(DATACON{name,vtype,rep=ref(VARIABLE _),const,...}) =>
		  (let val DATACON{vtype=vtype',rep=ref(VARIABLE(access)),...} =
			     lookEXN name
		    in compareTypes(vtype,newenv,vtype',name);
		       [VALtrans access]
		   end
		   handle Table.Notfound_Table =>
		     condemn ("sigMatch: missing exception "^symName(name)))
	       | CONbind(DATACON{name,vtype,...}) =>
		  (let val DATACON{vtype=vtype',...} = lookCON name
		    in compareTypes(vtype,newenv,vtype',name); nil
		   end
		   handle Table.Notfound_Table =>
		     condemn ("sigMatch: missing data constructor "^symName(name)))
	       | VARbind(VALvar{name,vtype,...}) =>
		  ((case (lookVARCON name)
		     of VARbind(VALvar{access,vtype=vtype',...}) =>
			  (* no propagation of INLINE access!! *)
			 (compareTypes(!vtype,newenv,!vtype',name);
 			  [case access of INLINE _ => VALtrans access
					| PATH _ => VALtrans access
					| _ => impossible "sigmatch.1"])
		      | CONbind(dcon as DATACON{vtype=vtype',...}) =>
			  (compareTypes(!vtype,newenv,vtype',name);
			   [CONtrans dcon])
		      | _ => impossible "sigmatch.476")
		   handle Table.Notfound_Table =>
		     condemn ("sigMatch: missing val "^symName(name)))
	       | _ => impossible "sigMatch--unexpected spec. "
        fun checkList (a::rest) =
	       (checkSpec a handle Syntax => nil) @ checkList rest
          | checkList nil = nil
	fun insttyc(sigtyc,strtyc) =
	    case sigtyc
	      of DATAtyc _ => strtyc
	       | _ => if fixedStamp(tycStamp sigtyc)
		      then strtyc
		      else setTycStamp(!genTycStamp(),sigtyc)
	and instenv() =
	    let fun foreachTyc i =
		    (update(tNew,i,insttyc(tSig sub i, tNew sub i));
		     foreachTyc(i+1))
	     in foreachTyc 0
		handle Subscript => ()
	    end

	val trans = checkList bindings
	val _ = Sharing.checkSharing(table,newenv,share)
        val str = 
	    if abs
	    then (instenv();
		  STRstr{stamp= !genStrStamp(),sign=sign,table=table,
			 env=newenv,kind=STRkind})
	    else STRstr{stamp=strStamp,sign=sign,table=table,env=newenv,kind=STRkind}
     in mapstr1(sigStamp,str);
	(str, trans)
    end
  | sigFill _ = impossible "783 in sigmatch"

fun fctApply(FUNCTOR{param,tycCount,body},arg) : Structure * thinning =
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
	     in update(strmap,stamp,str);
		for bindings mapbinding
	    end
          | mapstr _ = impossible "887 in sigmatch"
	fun insttyc(tyc) =
	    case tyc
	      of VARtyc{stamp,...} => tycmap sub stamp
	       | DATAtyc{stamp,name,params,dcons} =>
		   if stamp < tCount  (* param *)
		   then tycmap sub stamp
		   else if stamp < stampBase  (* generative *)
		   then (case tycmap sub stamp
			  of INDtyc _ =>
			       let val newdcons = ref []
				   val newtyc = mkDATAtyc(name,params,newdcons)
				   val _ = update(tycmap,stamp,newtyc)
				in newtyc
			       end
			       (* BUG -- not properly updating dcons ? *)
			   | tyc' => tyc')
		   else tyc
	       | TYPEtyc{stamp,name,params,def} =>
		   if stamp < stampBase  (* generative *)
		   then (case tycmap sub stamp
			  of INDtyc _ =>
			       let val newtyc = mkTYPEtyc(name,params,insttype def)
				in update(tycmap,stamp,newtyc);
				   newtyc
			       end
			   | tyc' => tyc')
		   else tyc
	       | _ => tyc
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
		     else STRstr{stamp=stamp',sign=sign,table=table,kind=kind,
				 env=instenv(env)}
		 end
	    else if stamp < stampBase  (* generative formal *)
	    then mkSTR(sign,table,instenv(env))
	    else str  (* absolute *)
	  | inststr (INDstr i) = impossible ("8484 in sigmatch: "^makestring i)
	and instenv({s,t}:strenv) =
	    {s=arraymap(inststr,s),t=arraymap(insttyc,t)}
	val (_,thin) = sigMatch(false,{mapstr=mapstr,mapstr1=mapstr1,maptyc=maptyc},
				param,arg)
	    (* discarding the instantiated parameter structure *)
     in (inststr body, thin)
    end

end (* local open Basics *)

end (* structure SigMatch *)

